import json
import argparse
import pandas as pd
from datasets import load_dataset

def create_json_entries(path_to_datasets_json_file, data_file_paths):
    # example path: ~/LLaMA-Factory/data/dataset_info.json

    # Check if the file exists
    if not os.path.exists(path_to_datasets_json_file):
        raise FileNotFoundError(f"The file {path_to_datasets_json_file} does not exist.")

    with open(path_to_datasets_json_file, 'r') as f:
        datasets_json = json.load(f)

    if "hctqa_real_only" in data_file_paths and "train_path" in data_file_paths["hctqa_real_only"] and "test_path" in data_file_paths["hctqa_real_only"]:
        datasets_json["hctqa_real_only"] = {
            "filename" : datasets_json["hctqa_real_only"]["train_path"],
            "eval_file_name" : datasets_json["hctqa_real_only"]["test_path"],
            "columns": {
                "prompt": "instruction",
                "response": "output"
            }
        }
    else:
        print("No valid paths provided for hctqa_real_only. Skipping this entry.")

    if "hctqa_real_and_synthetic" in data_file_paths and "train_path" in data_file_paths["hctqa_real_and_synthetic"] and "test_path" in data_file_paths["hctqa_real_and_synthetic"]:
        datasets_json["hctqa_real_and_synthetic"] = {
            "filename" : datasets_json["hctqa_real_and_synthetic"]["train_path"],
            "eval_file_name" : datasets_json["hctqa_real_and_synthetic"]["test_path"],
            "columns": {
                "prompt": "instruction",
                "response": "output"
            }
        }
    else:
        print("No valid paths provided for hctqa_real_and_synthetic. Skipping this entry.")

    # Save the updated JSON back to the file
    with open(path_to_datasets_json_file, 'w') as f:
        json.dump(datasets_json, f, indent=2)

    print(f"Updated {path_to_datasets_json_file} with new dataset entries.")
    return 

def convert_parquet_to_json(path_to_main_llama_factory_folder):
    dataset = load_dataset("qcri-ai/HCTQA")
    
    # Convert each split to a pandas DataFrame
    full_train_df = pd.DataFrame(dataset['train'])
    full_test_df = pd.DataFrame(dataset['test'])

    data = []
    ret_path_dict = {}
    # Filter real world datasets
    real_only_train_df = full_train_df[full_train_df["dataset_type"] == "realWorldHCTs"]
    real_only_test_df = full_test_df[full_test_df["dataset_type"] == "realWorldHCTs"]

    for variation in ["real_only", "real_and_synthetic"]:
        if variation == "real_only":
            train_df = real_only_train_df
            test_df = real_only_test_df
        else:
            train_df = full_train_df
            test_df = full_test_df
        
        # Make output paths
        train_json_file = os.path.join(path_to_main_llama_factory_folder, f"/data/hctqa_{variation}_train.json")
        test_json_file = os.path.join(path_to_main_llama_factory_folder, f"/data/hctqa_{variation}_test.json")

        def data_frame_to_json(df, json_file):
            data = []
            for _, row in df.iterrows():
                prompt_list = eval(row["prompt"])
                system_prompt = prompt_list[0]["content"]
                user_prompt = prompt_list[1]["content"]
                ## Remove our formatting from answer
                answer = row["answer"]
                answer = answer.replace("{","").replace("}","").replace("||", "|").replace("|", "||").strip()


                data.append({
                    "instruction": user_prompt,
                    "system": system_prompt,
                    "output": answer
                })
            return data
        
        # Convert train and test dataframes to JSON
        train_data = data_frame_to_json(train_df, train_json_file)
        test_data = data_frame_to_json(test_df, test_json_file)

        # Save to JSON files
        try:
            with open(train_json_file, 'w') as f:
                json.dump(train_data, f, indent=2)
        except Exception as e:
            print(f"Error writing to {train_json_file}: {e}")

        try:
            with open(test_json_file, 'w') as f:
                json.dump(test_data, f, indent=2)
        except Exception as e:
            print(f"Error writing to {test_json_file}: {e}")

        print(f"Converted {variation} dataset to JSON files: {train_json_file}, {test_json_file}")
        ret_path_dict[variation] = {
            "train_path": train_json_file,
            "test_path": test_json_file
        }
    return ret_path_dict

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Create JSON entries for LLaMA-Factory datasets.")
    parser.add_argument("--path_to_datasets_json_file", type=str, required=True, help="Path to the datasets JSON file.")
    parser.add_argument("--path_to_main_llama_factory_folder", type=str, required=True, help="Path to the main LLaMA-Factory folder.")
    args = parser.parse_args()

    # Create JSON entries
    data_file_paths = convert_parquet_to_json(args.path_to_main_llama_factory_folder)
    create_json_entries(args.path_to_datasets_json_file, data_file_paths)

    
# Example usage:
# python create_hctqa_in_alpacaJson.py --path_to_datasets_json_file ~/LLaMA-Factory/data/dataset_info.json --path_to_main_llama_factory_folder ~/LLaMA-Factory

