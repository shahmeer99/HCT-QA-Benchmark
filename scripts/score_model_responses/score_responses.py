import json
import re, os
import argparse
import pandas as pd

##########################
######### HELPER FUNCTIONS
##########################

def clean_gt_val(ground_truth_t):
    ground_truth = re.sub(r"(?<=\d),(?=\d)", "", ground_truth_t)
    ground_truth = ground_truth.lower().strip()
    ground_truth = [x.lower().strip() for x in ground_truth.replace("{", "").replace("}", "").replace("||", "|").split("|")]
    for j in range(len(ground_truth)):
        temp_val = ground_truth[j]
        for _ in range(5):
            temp_val = re.sub(r'(\d+),(\d+)', r'\1\2', temp_val)

        # Remove trailing .0, .00, .000, etc.
        temp_val = re.sub(r'(\d+)\.0+\b', r'\1', temp_val)
        temp_val = re.sub(r'(\d*\.\d*?[1-9])0+\b', r'\1', temp_val)

        ground_truth[j] = temp_val

    return ground_truth

def post_process_response(response: str) -> str:

    response = response.lower().strip()
    
    # Define the regex pattern to match any of the junk tokens
    junk_tokens = r"(?:<eos_token>|#input|# input|# solution:|#solution|# explanation:|#explanation|note:|table:)"
    
    # Find the position of the first junk token
    match = re.search(junk_tokens, response)
    
    # If a junk token is found, truncate the response before it
    if match:
        response = response[:match.start()]
    
    ### Check for double new line (we see this trend quite often before models start yapping)
    response = response.split("\n\n")[0]
    reponse = response.split("\n```\n")[0]

    # Remove '`' characters
    response = response.replace("`", "").replace("\n","")

    # Replace "," in numbers with "" (example: 1,000 -> 1000 & 1,232.23 -> 1232.23 & |1,000,000 -> |1000000)
    for i in range(5):
        response = re.sub(r'(\d+),(\d+)', r'\1\2', response)
    
    # Remove trailing .0, .00, .000, etc.
    response = re.sub(r'(\d+)\.0+\b', r'\1', response)
    response = re.sub(r'(\d*\.\d*?[1-9])0+\b', r'\1', response)

    return [x.strip() for x in response.lower().strip().split("||")]

def post_process_response_vision_models_only(response: str) -> str:

    response = response.lower().strip()
    
    # Define the regex pattern to match any of the junk tokens
    junk_tokens = r"(?:<eos_token>|#input|# input|# solution:|#solution|# explanation:|#explanation|note:|table:)"
    
    # Find the position of the first junk token
    match = re.search(junk_tokens, response)
    
    # If a junk token is found, truncate the response before it
    if match:
        response = response[:match.start()]
    
    ### Check for double new line (we see this trend quite often before models start yapping)
    response = response.split("\n\n")[0]
    reponse = response.split("\n```\n")[0]

    # Remove '`' characters
    response = response.replace("`", "").replace("\n","")

    # Replace "," in numbers with "" (example: 1,000 -> 1000 & 1,232.23 -> 1232.23 & |1,000,000 -> |1000000)
    for i in range(5):
        response = re.sub(r'(\d+),(\d+)', r'\1\2', response)

    # Remove trailing .0, .00, .000, etc.
    response = re.sub(r'(\d+)\.0+\b', r'\1', response)
    response = re.sub(r'(\d*\.\d*?[1-9])0+\b', r'\1', response)
    
    return response.lower().strip()

def post_process_response_format_variation(response: str) -> str:

    response = response.lower().strip()
    
    # Define the regex pattern to match any of the junk tokens
    junk_tokens = r"(?:<eos_token>|#input|# input|# solution:|#solution|# explanation:|#explanation|note:|table:)"
    
    # Find the position of the first junk token
    match = re.search(junk_tokens, response)
    
    # If a junk token is found, truncate the response before it
    if match:
        response = response[:match.start()]
    
    ### Check for double new line (we see this trend quite often before models start yapping)
    response = response.split("\n\n")[0]
    reponse = response.split("\n```\n")[0]

    # Remove '`' characters
    response = response.replace("`", "").replace("\n","")

    # Replace "," in numbers with "" (example: 1,000 -> 1000 & 1,232.23 -> 1232.23 & |1,000,000 -> |1000000)
    for i in range(5):
        response = re.sub(r'(\d+),(\d+)', r'\1\2', response)

    values = [x.strip() for x in response.lower().strip().split("||")]
    new_values = []
    for value in values:
        # Remove trailing .0, .00, .000, etc.
        new_value = re.sub(r'(\d+)\.0+\b', r'\1', value)
        new_value = re.sub(r'(\d*\.\d*?[1-9])0+\b', r'\1', new_value)
        new_values.append(new_value)
        
    return new_values
    
def get_prec_rec_f1_cc(results):
    cleaned_gts = [clean_gt_val(x['gt']) for x in results]
    cleaned_responses = [post_process_response(x['response']) for x in results]

    # For each response and GT pair calculate precision and recall
    def calculate_precision_recall(gt, response):
        gt_set = set(gt)
        response_set = set(response)
        
        # Calculate precision and recall
        precision = sum(1 for x in response_set if x in gt_set) / max(len(response_set), 1)
        recall = sum(1 for x in gt_set if x in response_set) / max(len(gt_set), 1)
        
        return precision, recall

    def calculate_f1(precision, recall):
        if precision + recall == 0:
            return 0
        else:
            return (2 * precision * recall) / (precision + recall)

    # Calculate precision, recall, and F1 score for each response
    precision_list = []
    recall_list = []
    f1_list = []
    cc_list = []
    for i in range(len(cleaned_responses)):
        precision, recall = calculate_precision_recall(cleaned_gts[i], cleaned_responses[i])
        f1 = calculate_f1(precision, recall)
        
        precision_list.append(precision)
        recall_list.append(recall)
        f1_list.append(f1)
        if recall == 1:
            cc_list.append(1)
        else:
            cc_list.append(0)
    
    # Calculate average precision, recall, F1 and cc score
    avg_precision = sum(precision_list) / len(precision_list)
    avg_recall = sum(recall_list) / len(recall_list)
    avg_f1 = sum(f1_list) / len(f1_list)
    avg_cc = sum(cc_list) / len(cc_list)

    return avg_precision, avg_recall, avg_f1, avg_cc


def results_to_per_dataset_results(results):
    # Create a dictionary to hold the results for each dataset
    dataset_results = {}

    # Iterate through the results and group them by dataset
    for result in results:
        dataset = result['id'].split("--")[0]
        if dataset not in dataset_results:
            dataset_results[dataset] = [result]
        else:
            dataset_results[dataset].append(result)
    return dataset_results


def get_rec_cc_vision(results):
    
    # For each response and GT pair calculate precision and recall
    def calculate_recall(gt, response):
        gt_set = set(gt)
        # Calculate precision and recall
        recall = sum(1 for x in gt_set if x in response) / max(len(gt_set), 1)
        return recall
    
    # Calculate Recall and CC score for each response
    cleaned_gts = [x['gt'] for x in results]
    cleaned_responses = [post_process_response_vision_models_only(x['response']) for x in results]
    
    recall_list = []
    cc_list = []

    for i in range(len(results)):
        recall = calculate_recall(cleaned_gts[i], cleaned_responses[i])
        recall_list.append(recall)
        if recall == 1:
            cc_list.append(1)
        else:
            cc_list.append(0)
    
    # Calculate average precision, recall, F1 and cc score
    avg_recall = sum(recall_list) / len(recall_list)
    avg_cc = sum(cc_list) / len(cc_list)

    return avg_recall, avg_cc

def results_to_per_dataset_results_vision(results):
    # Create a dictionary to hold the results for each dataset
    dataset_results = {}
    # Iterate through the results and group them by dataset
    for result in results:
        dataset = result['id'].split("--")[0]
        if dataset not in dataset_results:
            dataset_results[dataset] = [result]
        else:
            dataset_results[dataset].append(result)
    return dataset_results

def get_recall_cc_vision_results(fname):
    all_results = {}
    ### Load File 
    with open(fname) as f:
        lines = f.readlines()
        lines = [json.loads(x) for x in lines]

    ### Reference GTs
    with open("./helper_for_vision_scoring.json") as f:
        helper_lines = json.load(f)

    ids_to_gts = {x["id"]: clean_gt_val(x['gt']) for x in helper_lines}

    # Do this filtering because these results include ones with bad ground truths and bad questions as well
    final_lines = []
    for line in lines:
        try:
            line["gt"] = ids_to_gts[line["id"]]
            final_lines.append(line)
        except:
            continue
    
    all_recall, all_cc = get_rec_cc_vision(final_lines)
    all_results["ALL"] = {}
    all_results["ALL"]["recall"] = all_recall
    all_results["ALL"]["cc"] = all_cc

    per_data_lines = results_to_per_dataset_results_vision(final_lines)

    for dataset, lines in per_data_lines.items():
        dataset_recall, dataset_cc = get_rec_cc_vision(lines)
        all_results[dataset] = {}
        all_results[dataset]["recall"] = dataset_recall
        all_results[dataset]["cc"] = dataset_cc

    return all_results


############################
######### MAIN SCORING FUNCS
############################

def score_vision_only_results(folder_name, output_file, print_results=False):
    output_string = ""

    for fname in os.listdir(folder_name):
        if "vision" not in fname:
            continue
        full_fname = os.path.join(folder_name, fname)
        all_results = get_recall_cc_vision_results(full_fname)

        for dataset, results in all_results.items():
            model_name = fname.split("--")[0]
            output_string += f"Model: {model_name} on {dataset}\n"
            output_string += f"Mean Recall: {results['recall']:.4f}\n"
            output_string += f"Mean CC Score: {results['cc']:.4f}\n"
            output_string += "*" * 50
        
    with open(output_file, "w") as f:
        f.write(output_string)

    if print_results:
        print(output_string)

    print(f"Results saved to {output_file}")
    return

def score_realHCT_text_only_results(folder_name, output_file, print_results=False):
    output_string = ""

    for fname in os.listdir(folder_name):
        if "real"  not in fname:
            continue
        with open(os.path.join(folder_name, fname)) as f:
            results = json.load(f)

        model_name = fname.split("--")[0]
        data_mode = fname.split("--")[1]
        mean_precision, mean_recall, mean_f1, mean_cc = get_prec_rec_f1_cc(results)

        output_string += f"Model: {model_name} on {data_mode} on dataset ALL\n"
        output_string += f"Mean Precision: {mean_precision:.4f}\n"
        output_string += f"Mean Recall: {mean_recall:.4f}\n"
        output_string += f"Mean F1 Score: {mean_f1:.4f}\n"
        output_string += f"Mean CC Score: {mean_cc:.4f}\n"
        output_string += "*" * 50

        dataset_results = results_to_per_dataset_results(results)

        for dataset, results_d in dataset_results.items():
            model_name = fname.split("--")[0]
            data_mode = fname.split("--")[1]
            mean_precision, mean_recall, mean_f1, mean_cc = get_prec_rec_f1_cc(results_d)

            output_string += f"Model: {model_name} on {data_mode} on dataset {dataset}\n"
            output_string += f"Mean Precision: {mean_precision:.4f}\n"
            output_string += f"Mean Recall: {mean_recall:.4f}\n"
            output_string += f"Mean F1 Score: {mean_f1:.4f}\n"
            output_string += f"Mean CC Score: {mean_cc:.4f}\n"
            output_string += "*" * 50

    with open(output_file, "w") as f:
        f.write(output_string)

    if print_results:
        print(output_string)

    print(f"Results saved to {output_file}")
    return


def score_syntheticHCT_text_only_results(folder_name, output_file, print_results=False):
    output_string = ""

    for fname in os.listdir(folder_name):
        if "synthetic" not in fname:
            continue
        output_string += f"{fname}\n"
        try:
            with open(os.path.join(folder_name, fname)) as f:
                results = json.load(f)
            model_name = fname.split("--")[0]
            data_mode = fname.split("--")[1]
            mean_precision, mean_recall, mean_f1, mean_cc = get_prec_rec_f1_cc(results)

            output_string += f"Model: {model_name} on {data_mode}\n"
            output_string += f"Mean Precision: {mean_precision:.4f}\n"
            output_string += f"Mean Recall: {mean_recall:.4f}\n"
            output_string += f"Mean F1 Score: {mean_f1:.4f}\n"
            output_string += f"Mean CC Score: {mean_cc:.4f}\n"
            output_string += "*" * 50

        except Exception as e:
            output_string += "Error\n"
            output_string += "*" * 50

    with open(output_file, "w") as f:
        f.write(output_string)

    if print_results:
        print(output_string)

    print(f"Results saved to {output_file}")
    return

############ MAIN

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Score model responses")
    parser.add_argument("--folder_name", type=str, required=True, help="Folder containing the model response files")
    parser.add_argument("--output_file", type=str, required=True, help="Output file to save the results")
    parser.add_argument("--print_results", action="store_true", help="Print results to console")
    parser.add_argument("--mode", type=str, required=True, choices=["vision", "real", "synthetic"], help="Mode to run the scoring for")
    args = parser.parse_args()
    folder_name = args.folder_name
    output_file = args.output_file
    print_results = args.print_results
    mode = args.mode
    if mode == "vision":
        score_vision_only_results(folder_name, output_file, print_results)
    elif mode == "real":
        score_realHCT_text_only_results(folder_name, output_file, print_results)
    elif mode == "synthetic":
        score_syntheticHCT_text_only_results(folder_name, output_file, print_results)
    else:
        raise ValueError("Invalid mode. Choose from 'vision', 'real', or 'synthetic'.")
    print(f"Results saved to {output_file}")
    return

# Example usage:
# python score_responses.py --folder_name /path/to/folder --output_file /path/to/output.txt --print_results --mode vision

