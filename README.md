# **Human-Centric Tables (HCT) Question Answering Benchmark**

## **Introduction**
This repository provides a benchmark for question answering on **Human-Centric Tables (HCTs)**. HCTs are non-relational tables with complex structures, making them difficult to parse and understand. This benchmark enables the evaluation of **large language models (LLMs) and vision-language models (VLMs)** on both real-world and synthetic HCTs.

Example of HCT:

![Example of HCT](https://hcsdtables.qcri.org/datasets/all_images/psa_10_336.jpg)


Details of the benchmark methodology and dataset can be found in our upcoming paper (**link to be added**).

---

## **Repository Structure**

```
├── datasets/                    # Real-world dataset
│   ├── qaps/                    # Question-answer pairs
│   ├── prompts/                 # Prompts used in experiments
│   ├── tables/                  # HCT images and CSVs (compressed as .gz files)
│
├── synthetic_data_generator/    # Synthetic HCT generation (has its own README)
│   ├── ...                      # Scripts (in R) needed to run synthetic generator
│   ├── README_SYNTHETIC_GENERATOR.md
│
├── inference_evaluate/          # Main experiment scripts
│   ├── llm_inference.py         # Runs inference with LLMs
│   ├── vlm_mlm_inference.py     # Runs inference with VLMs
│   ├── make_prompts_file.py     # Creates prompt files from QAPs & table CSVs
│   ├── llm_query.py             # Helper script for LLM inference
│   ├── score_model_responses.ipynb # Evaluates responses against ground truth
│   ├── to_run.sh                 # Script to run all experiments
│
├── results/                     # Example model responses (subset only)
│   ├── model_responses/
│
├── requirements.txt             # Dependencies for the benchmark
├── format_files.sh              # Script to prepare and uncompress data
└── README.md                    # This README file
```

---

## **Data Components**

### **Real-World Data Processing** (`datasets/`)
- **`qaps/`**: Contains question-answer pairs.
- **`prompts/`**: Prompt templates used for model inference.
- **`tables/`**: HCTs provided as **compressed** `.gz` files (CSV and images).

Ground Truth Format:
The `gt` attribute in the prompts and qaps files present the answer in the following format:
- Values from the same row are encased in `{}` and within that values from different columns are separated by `|`
- Values from different rows are separated by `||` 
- Aggregations are put in `{}` and multiple distinct aggregations are separated by `||`

This format allows for more detailed evaluation of the models.

To extract the data:
```bash
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
```bash
conda create --name hct_benchmark python=3.12 -y
conda activate hct_benchmark
```

Install dependencies:
```bash
pip install -r requirements.txt
```

#### **Set Up Hugging Face Token**
Some models require access via **Hugging Face**. Create a `.env` file:
```bash
echo "HUGGINGFACE_TOKEN=your_token_here" > .env
```
Replace `your_token_here` with your actual **Hugging Face API Token**.

---

### **2. Running the Experiment**

The main script to run the full experiment is `to_run.sh`, which includes example usages for running LLM inference on real-world and synthetic datasets, as well as VLM inference.

#### **Run the full experiment**
```bash
chmod +x to_run.sh
./to_run.sh
```
This script will:
- Run LLM inference on real-world data.
- Run LLM inference on synthetic datasets.
- Run VLM inference on real-world data.
- Set up the environment and parameters automatically.

For more details, inspect `to_run.sh` to customize it for your use case.

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
