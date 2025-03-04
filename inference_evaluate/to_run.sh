### Load Conda Env

### Change to directory 
cd /export/home/mshahmmer/hcsd_github_reformat/latest_inf_analysis/Final_Files_For_Git/actual_git_repo/HCTQA-Benchmark/inference_evaluate/

#################### Set Input Paramss - SET YOURSELF USING ABSOLUTE PATHS
realWorld_qaps_file_path = "../realWorld_data_processing/realWorld_datasets/qaps/realWorld_HCT_qaps.json"
synthetic_qaps_file_path = "../realWorld_data_processing/realWorld_datasets/qaps/synthetic_HCT_qaps.json"

realWorld_prompts_file_path = "../realWorld_data_processing/realWorld_datasets/prompts/realWorld_HCT-all_prompts.jsonl"
synthetic_prompts_file_path = "../realWorld_data_processing/realWorld_datasets/prompts/synthetic_HCT-all_prompts.jsonl"

inference_results_path = "../results/model_responses/"
scores_results_path = "../results/scores/"

batch_size_for_inference = 128

#################### Make Prompts File

## Real World Tables
python3 make_prompts_file.py --input_file $realWorld_qaps_file_path --output_file $realWorld_prompts_file_path

## Synthetic Tables
python3 make_prompts_file.py --input_file $synthetic_qaps_file_path --output_file $synthetic_prompts_file_path

#################### Run Inference

## MEDIUM MODELS
declare -A medium_models=(
    ["mistralai/Mixtral-8x7B-v0.1"]=8192
    ["meta-llama/Llama-3.1-70B-Instruct"]=8192
)

for model_path in "${!medium_models[@]}"; do
    max_model_len=${medium_models[$model_path]}
    python generic_inference_for_folder_of_prompts.py \
        --prompts_folder $realWorld_prompts_file_path \
        --out_file_path $inference_results_path \
        --use_system_prompt true \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 2 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference

### SMALL MODELS
declare -A small_models=(
    ["meta-llama/Llama-2-7b-chat-hf"]=4096
    ["meta-llama/Meta-Llama-3-8B-Instruct"]=8192
    ["meta-llama/Meta-Llama-3.1-8B-Instruct"]=15000
    ["google/gemma-2-9b-it"]=4096
    ["microsoft/phi-2"]=2048
    ["microsoft/Phi-3-mini-4k-instruct"]=4096
    ["microsoft/Phi-3.5-mini-instruct"]=4096
    ["mistralai/Mistral-7B-Instruct-v0.3"]=8192
    ["CohereForAI/aya-23-8B"]=8192
    ["lmsys/vicuna-7b-v1.5"]=4096
    ["Qwen/Qwen1.5-7B-Chat"]=8192
    ["Qwen/Qwen2-7B-Instruct"]=8192
    ["mistralai/Mathstral-7B-v0.1"]=8192
)

for model_path in "${!small_models[@]}"; do
    max_model_len=${small_models[$model_path]}
    python generic_inference_for_folder_of_prompts.py \
        --prompts_folder $synthetic_prompts_file_path \
        --out_file_path $inference_results_path \
        --use_system_prompt true \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 2 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference

### GEMMA MODELS
declare -A gemma_models=(
    ["google/gemma-2-9b-it"]=4096
    ["google/gemma-2-27b-it"]=4096
)

for model_path in "${!gemma_models[@]}"; do
    max_model_len=${gemma_models[$model_path]}
    python generic_inference_for_folder_of_prompts.py \
        --prompts_folder $synthetic_prompts_file_path \
        --out_file_path $inference_results_path \
        --use_system_prompt false \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 2 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference


#################### Score Inference Results
### Use notebook @ `./HCTQA-Benchmark/inference_evaluate/score_model_responses.ipynb`

done


