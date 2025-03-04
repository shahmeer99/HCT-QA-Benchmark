import sys, os, torch, argparse, json, math, time
from vllm.utils import cuda_device_count_stateless
from huggingface_hub import login
from llm_query import llm_query

# Load .env file
from dotenv import load_dotenv
load_dotenv()

login(token=os.getenv("HUGGINGFACE_TOKEN"))

os.environ['VLLM_WORKER_MULTIPROC_METHOD'] = 'spawn' # https://github.com/vllm-project/vllm/issues/6152

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # Necessary Arguments
    parser.add_argument('--prompts_folder', type=str, default='', help="Path to file containing all the prompts.")
    parser.add_argument('--output_folder', type=str, default='', help="Path to file to write results to.")
    parser.add_argument('--use_system_prompt', type=bool, default=True, help="Whether to use system prompt or not.")

    # Model Params
    parser.add_argument('--model_path', type=str, default='')
    parser.add_argument('--batch_size', type=int, default=128)
    parser.add_argument('--max_tokens', type=int, default=10000)
    parser.add_argument('--max_model_len', type=int, default=15000)
    parser.add_argument('--tensor_parallel_size', type=int, default=1)
    parser.add_argument('--gpu_memory_utilization', type=float, default=0.9)
    
    args = parser.parse_args()

    ## Make output folder if it does not exist
    if not os.path.exists(args.output_folder):
        os.makedirs(args.output_folder)

    ## Load LLM Model
    llm = llm_query(model_path=args.model_path, sys_prompt=None, 
        gpu_memory_utilization=args.gpu_memory_utilization, 
        tensor_parallel_size=args.tensor_parallel_size,
        max_model_len=args.max_model_len) #, distributed_executor_backend='mp')
    
    print("LOADED MODEL: ", args.model_path)

    ## Ensure the seed file exists
    if not os.path.exists(args.prompts_folder):
        raise Exception('File does not exist:', args.prompts_file)

    ## Open prompts json file
    for fname in os.listdir(args.prompts_folder):
        print(fname)
        if "ignore" in fname or "IGNORE" in fname:
            continue
        elif args.use_system_prompt and "--without_system_prompts" in fname:
            continue
        elif args.use_system_prompt == False and "--without_system_prompts" not in fname:
            continue
        elif fname.endswith('.json'):
            with open(os.path.join(args.prompts_folder, fname), 'r', encoding='utf-8') as f:
                seeds = json.load(f)
        elif fname.endswith('.jsonl'):
            print(f"Loading {fname} as jsonl")
            with open(os.path.join(args.prompts_folder, fname), 'r', encoding='utf-8') as f:
                seeds = [json.loads(line) for line in f]
    
        ## load the data
        print('++ Processing:', fname)

        ## Separate the seeds into batches
        N = len(seeds)
        n_splits = int(math.ceil(N/args.batch_size))

        print('Total:', N, 'Batches:', n_splits)

        ## Main Loop for this task
        t_start = time.time()
        all_outputs = []
        for i in range(n_splits):
            t0 = time.time()
            start = i*args.batch_size
            end = min((i+1)*args.batch_size,N)

            batch = seeds[start:end]

            user_prompts = [seed_t['prompt'] for seed_t in batch]

            t_beg = time.time()
            results = llm.chat(user_prompts=user_prompts, temperature=0.01, repetition_penalty=1.1, max_tokens=args.max_tokens)
            print('Batch:', i, 'Time:', time.time()-t_beg)

            for j in range(len(results)):
                all_outputs.append({
                    'qap_id': batch[j]['qap_id'],
                    'response': results[j],
                    "gt" : batch[j]['gt']
                })
            
            # Extract texts
            for out in all_outputs:
                try:
                    out["response"] = out["response"].outputs[0].text.strip()
                except:
                    pass
            
        # save the results by appending each batch to output_folder
        model_name = args.model_path.split('/')[-1]
        prompt_dataset_type = fname.replace(".json", "").replace(".jsonl", "")
        outfile_path = os.path.join(args.output_folder, f"results--{model_name}--{prompt_dataset_type}.jsonl")
        with open(outfile_path, 'a', encoding='utf-8') as f:
            for out in all_outputs:
                f.write(json.dumps(out)+'\n')
            
        t_end = time.time()
        print(f'++ Total time for task {args.model_path}:', t_end-t_start)
        print('Done.\n ********************* \n ********************* \n')