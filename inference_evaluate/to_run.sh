### THIS SCRIPT REQUIRES 4 A100's 80GB VRAM to run (change batch size to adjust this GPU usage)

### Activate conda environment 
# conda activate <path_to_your_conda_env>

### Change to directory 
#################### Set Input Paramss - SET YOURSELF USING ABSOLUTE PATHS

realWorld_qaps_file_path="../realWorld_data_processing/realWorld_datasets/qaps/realWorld_HCT_qaps.json"
realWorld_prompts_file_path="../realWorld_data_processing/realWorld_datasets/prompts/realWorld_HCT-all_prompts.jsonl"

llm_text_inference_results_path="../results/model_responses/llms/"
vlm_text_inference_results_path="../results/model_responses/vlms/"

scores_results_path="../results/scores/"

batch_size_for_inference=128 # Change according to resource availability

####################################################################################################################
#################### Make Prompts File - Already Run 
####################################################################################################################

### Real World Tables
# python3 make_prompts_file.py --input_file $realWorld_qaps_file_path --output_file $realWorld_prompts_file_path

### Synthetic Tables  - IGNORE
# python3 make_prompts_file.py --input_file $synthetic_qaps_file_path --output_file $synthetic_prompts_file_path

####################################################################################################################
#################### Run Inference - LLMs on Text Modality - Real World Tables
####################################################################################################################
## MEDIUM MODELS
declare -A medium_models=(
    ["mistralai/Mixtral-8x7B-v0.1"]=8192
    ["meta-llama/Llama-3.1-70B-Instruct"]=8192
)

for model_path in "${!medium_models[@]}"; do
    max_model_len=${medium_models[$model_path]}
    python llm_inference.py \
        --prompts_file $realWorld_prompts_file_path \
        --output_folder $llm_text_inference_results_path \
        --use_system_prompt true \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 4 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference
done

## SMALL MODELS
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
    python llm_inference.py \
        --prompts_file $realWorld_prompts_file_path \
        --output_folder $llm_text_inference_results_path \
        --use_system_prompt true \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 2 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference
done

### GEMMA MODELS
declare -A gemma_models=(
    ["google/gemma-2-9b-it"]=4096
    ["google/gemma-2-27b-it"]=4096
)

for model_path in "${!gemma_models[@]}"; do
    max_model_len=${gemma_models[$model_path]}
    python llm_inference.py \
        --prompts_file $realWorld_prompts_file_path \
        --output_folder $llm_text_inference_results_path \
        --use_system_prompt false \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 2 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference
done

####################################################################################################################
#################### Run Inference - VLMs on Image Modality
####################################################################################################################
# "OpenGVLab/InternVL2_5-8B-MPO",
python3 vlm_mlm_inference.py --model "OpenGVLab/InternVL2_5-8B-MPO" --output_folder $vlm_text_inference_results_path

# # "OpenGVLab/InternVL2-4B",
python3 vlm_mlm_inference.py --model "OpenGVLab/InternVL2-4B" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path

# # "llava-hf/llava-1.5-7b-hf",
python3 vlm_mlm_inference.py --model "llava-hf/llava-1.5-7b-hf" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path

# # "llava-hf/llava-v1.6-mistral-7b-hf",
python3 vlm_mlm_inference.py --model "llava-hf/llava-v1.6-mistral-7b-hf" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path

# # "llava-hf/llava-v1.6-vicuna-7b-hf",
python3 vlm_mlm_inference.py --model "llava-hf/llava-v1.6-vicuna-7b-hf" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path

# # "allenai/Molmo-7B-D-0924",
python3 vlm_mlm_inference.py --model "allenai/Molmo-7B-D-0924" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path
    
# # "google/paligemma2-10b-ft-docci-448",
python3 vlm_mlm_inference.py --model "google/paligemma2-10b-ft-docci-448" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path

# # "mistralai/Pixtral-12B-2409",
python3 vlm_mlm_inference.py --model "mistralai/Pixtral-12B-2409" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path

# # "microsoft/Phi-3-vision-128k-instruct",
python3 vlm_mlm_inference.py --model "microsoft/Phi-3-vision-128k-instruct" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path
    
# # "microsoft/Phi-3.5-vision-instruct",
python3 vlm_mlm_inference.py --model "microsoft/Phi-3.5-vision-instruct" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path
    
# # "Qwen/Qwen2-VL-7B-Instruct"
python3 vlm_mlm_inference.py --model "Qwen/Qwen2-VL-7B-Instruct" --output_folder $vlm_text_inference_results_path --qaps_file $realWorld_qaps_file_path

# ####################################################################################################################
# #################### Run Inference - LLMs on Text Modality - Synthetic Tables
# ####################################################################################################################

# ## MEDIUM MODELS
declare -A medium_models=(
    ["mistralai/Mixtral-8x7B-v0.1"]=8192
    ["meta-llama/Llama-3.1-70B-Instruct"]=8192
)

for model_path in "${!medium_models[@]}"; do
    max_model_len=${medium_models[$model_path]}
    python llm_inference.py \
        --prompts_file $synthetic_prompts_file_path \
        --output_folder $llm_text_inference_results_path \
        --use_system_prompt true \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 4 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference
done

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
    python llm_inference.py \
        --prompts_file $synthetic_prompts_file_path \
        --output_folder $llm_text_inference_results_path \
        --use_system_prompt true \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 2 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference
done

### GEMMA MODELS
declare -A gemma_models=(
    ["google/gemma-2-9b-it"]=4096
    ["google/gemma-2-27b-it"]=4096
)

for model_path in "${!gemma_models[@]}"; do
    max_model_len=${gemma_models[$model_path]}
    python llm_inference.py \
        --prompts_file $synthetic_prompts_file_path \
        --output_folder $llm_text_inference_results_path \
        --use_system_prompt false \
        --model_path "$model_path" \
        --gpu_memory_utilization 0.9 \
        --tensor_parallel_size 2 \
        --max_tokens 1000 \
        --max_model_len "$max_model_len" \
        --batch_size $batch_size_for_inference
done

####################################################################################################################
#################### Score Inference Results
####################################################################################################################

### Use notebook @ `./HCTQA-Benchmark/inference_evaluate/score_model_responses.ipynb`


