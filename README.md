# ** Paper: HCT-QA: A Benchmark for Question Answering on Human-Centric Tables**

## **Introduction**
This repository provides a benchmark for question answering on **Human-Centric Tables (HCTs)**. HCTs are non-relational tables with complex structures, making them difficult to parse and understand. This benchmark enables the evaluation of **large language models (LLMs) and vision-language models (VLMs)** on both real-world and synthetic HCTs.

Example of HCT:

![Example of HCT](https://hcsdtables.qcri.org/datasets/all_images/psa_10_336.jpg)


Details of the benchmark methodology and dataset can be found in our upcoming paper (**link to be added**).

---

## **Repository Structure**

```
├── datasets/realWorld_datasets/ # Real-world dataset
│   ├── qaps/                    # Question-answer pairs
│   ├── prompts/                           # Prompts used in experiments
│   ├── tables/                            # HCT images and CSVs (compressed as .gz files)
│
├── synthetic_data_generator/                # Synthetic HCT generation (has its own README)
│   ├── ...                                  # Scripts (in R) needed to run synthetic generator
│   ├── README_SYNTHETIC_GENERATOR.md
│
├── scripts/                                 # All scripts
│   ├── inference_experiments/               # Runs inference experiments from paper
│   │   ├── llm_inference/
│   │   ├── vlm_inference/
│   ├── score_model_responses/               # Runs inference with VLMs
│   │   ├── score_responses.py               # Script to score all model responses (llm + vlm)
│   ├── finetuning/                          # Skeleton folder to set up LLAMA-FACTORY in
│   │   ├── datatset_prep_for_llama_factory/
│   │   ├── config_yamls/
│
├── results/                                 # Example model responses (subset only)
│   ├── model_responses/
│   ├── scores/
│
├── requirements.txt                         # Dependencies for the benchmark
├── format_files.sh                          # Script to prepare and uncompress data
└── README.md                                # This README file
```

---

## **Data Components**

### **Real-World Data Processing** (`datasets/`)
- **`qaps/`**: Contains question-answer pairs.
- **`tables/`**: HCTs provided as **compressed** `.gz` files (CSV and images).

#### The data is also available in [`HuggingFace`](https://huggingface.co/datasets/qcri-ai/HCTQA)
```python
<pre> ```python from datasets import load_dataset dataset = load_dataset("qcri-ai/HCTQA") ``` </pre>

```

**Ground Truth Format**:
The `gt` attribute in the prompts and qaps files present the answer in the following format:
- Values from the same row are encased in `{}` and within that values from different columns are separated by `|`
- Values from different rows are separated by `||` 
- Aggregations are put in `{}` and multiple distinct aggregations are separated by `||`

This format allows for more detailed evaluation of the models.

To extract the table CSVs and Images run:
```setup
chmod +x ./format_files.sh
./format_files.sh
```

### **Synthetic Data Generation** (`synthetic_data_generator/`)
This module allows users to generate synthetic HCTs with different styles and properties for experimentation.
Refer to [`synthetic_data_generator/README_SYNTHETIC_GENERATOR.md`](synthetic_data_generator/README_SYNTHETIC_GENERATOR.md) for details. Details of the template types used to create the questions for these synthetic tables can also be found in that README file.

---

## **Running the Benchmark**

### **1. Setup & Installation**

We recommend using **Python 3.12** and a virtual environment (e.g., Conda):
```setup
conda create --name hct_benchmark python=3.12 -y
conda activate hct_benchmark
```

Install dependencies:
```setup
pip install -r requirements.txt
```

#### **Set Up Hugging Face Token**
Some models require access via **Hugging Face**. Create a `.env` file in the "HCT-QA-Benchmark" folder:
```setup
echo "HUGGINGFACE_TOKEN=<your_token_here>" > .env
```
Replace `<your_token_here>` with your actual **Hugging Face API Token**.

---

### **2. Running the Experiments**

To run experiments with text-only LLMs run:  
```bash
cd /scripts/inference_experiments/llm_inference

python llm_inf.py --model_name_or_path "google/gemma-3-12b-it" --output_folder "../../results/model_responses/llms/" --data_source_type "real" --split_name "all" --batch_size 32 --num_gpus 1 --use_system_prompt True
```

The parameters for this command are:
| Parameter | Description |
|-----------|-------------|
| `--model_name_or_path` | Path to local or huggingface model. "all" for models used in the paper. |
| `--output_folder` | Path to folder where model responses will be stored.|
| `--data_source_type` | "real", "synthetic", "all" to choose what type of HCTs to run inference on. |
| `--split_name` | "train", "validation", "test" or "all" to choose which split of data to run inference on |
| `--batch_size` | Batch size for inference |
| `--num_gpus` | Number of available GPUs to run parallel inference on, default = 1. |
| `--use_system_prompt` | Boolean to determine whether to use system prompt or not (some models like gemma-2 do not support system prompting) |

To run experiments with vision-text VLMs run:  
```bash
cd /scripts/inference_experiments/llm_inference

python vllm_inference.py --model "meta-llama/Llama-3.2-11B-Vision-Instruct" --num_gpus 2 
```

The parameters for this command are:
| Parameter | Description |
|-----------|-------------|
| `--model` | Path to local or huggingface model. "all" for models used in the paper. |
| `--output_folder` | Path to folder where model responses will be stored.|
| `--qaps_file` | path to the qaps file (normally in "datasets/realWorld_datasets/qaps") |
| `--num_gpus` | Number of available GPUs to run parallel inference on, default = 1|

## Finetuned Models

We use (LLAMA-Factory)[https://github.com/hiyouga/LLaMA-Factory] to fine-tune our models. 

Here are the steps to replicate the training:

1. Clone LLAMA-Factory repository
```bash
git clone https://github.com/hiyouga/LLaMA-Factory.git
```

2. Create custom dataset for finetuning using the script we provide
```bash
cd /scripts/finetuning/datatset_prep_for_llama_factory

python3 create_hctqa_in_alpacaJson.py --path_to_datasets_json_file ~/LLaMA-Factory/data/dataset_info.json --path_to_main_llama_factory_folder ~/LLaMA-Factory
```
For the `--path_to_datasets_json_file` argument provide the path to the dataset_info.json file in your lcoal cloned LLAMA-Factory directory. This file should already exist when you clone.   
For the `--path_to_main_llama_factory_folder` argument provide the path to the main LLaMA-Factory folder when you clone the repo (the parent folder that contains all their files anc code).  

3. Create a config.yaml file for finetuning. Example files are provided in `/scripts/finetuning/config_yamls/`

4. Copy your config.yaml file into the main `/LLaMA-Factory` folder inside your cloned repo. Do not skip this step as the LLaMA-Factory scripts expect this config to be in the same directory.

5. Run the train command (from the inside the /LLaMA-Factory folder):
```bash
llamafactory-cli train <path to your config.yaml that should be in the main /LLaMA-Factory folder
```
  
---

## **Planned Updates & Future Work**

- **Leaderboard:** Track model performance on HCT-QA.
- **Dataset on Hugging Face:** Public dataset release for easy access.
- **Expanding the Dataset:** More HCTs from diverse sources & domains.
- **Question Difficulty Classification:** A framework for automatic difficulty scoring.

Stay tuned for updates!

---

## **Citing & Contribution**

If you use this benchmark, please cite our work (citation details will be added once the paper is published).

### **Contributions**
We welcome contributions! Please submit issues or pull requests to improve the benchmark.

---

## **License**

This repository is licensed under the MIT License.

---
