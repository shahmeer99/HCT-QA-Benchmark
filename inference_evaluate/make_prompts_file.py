import os, json
import argparse

def make_prompts_file_from_qaps_file(input_qaps_file, output_file_name):

    with open(input_qaps_file, "r") as f:
        the_qaps_dict = json.load(f)

    system_prompt = "You are a table question answering assistant. You are given a table and a question. Your task is to answer the question based on the information in the table. The table structure may be complex and not a standard relational table so try to understand the structure of the table when answering the question. If the question cannot be answered using information from the table, return 'No Answer'. Do not provide any explainations, equations, code, or text explaning intermediate steps in figuring out the answer. Return only the final answer itself. If there are multiple values, separate each value with the ' || ' token."
    user_prompt = "Answer the question given the input table. Do not provide any explainations, equations, code, or text explaning intermediate steps in figuring out the answer. Return only the final answer itself.\n # INPUTS: \nTable: \n `{table_t}` \n\nQuestion: `{question_t}`\n\n # OUTPUT: \nAnswer:"
    system_user_prompt_merged =  "You are a table question answering assistant. You are given a table and a question. Your task is to answer the question based on the information in the table. The table structure may be complex and not a standard relational table so try to understand the structure of the table when answering the question. If the question cannot be answered using information from the table, return 'No Answer'. Do not provide any explainations, equations, code, or text explaning intermediate steps in figuring out the answer. Return only the final answer itself. If there are multiple values, separate each value with the ' || ' token. Answer the question given the input table. Do not provide any explainations, equations, code, or text explaning intermediate steps in figuring out the answer. Return only the final answer itself.\n # INPUTS: \nTable: \n `{table_t}` \n\nQuestion: `{question_t}`\n\n # OUTPUT: \nAnswer:"

    prompts_list, prompts_without_system_prompt_list = [], []

    for tab_id, info_t in the_qaps_dict.items():
        for question_info in info_t["questions"]:
            messages_t = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt.format(table_t=info_t["table_info"]["table_formats"]['csv'], question_t=question_info["question"])}
            ]
            messages_t_without_system = [
                {"role": "user", "content": system_user_prompt_merged.format(table_t=info_t["table_info"]["table_formats"]['csv'], question_t=question_info["question"])}
            ]

            qap_id_t = question_info["question_id"]
            table_id_t = tab_id
            gt_t = question_info["gt"]

            prompts_list.append({
                "qap_id": qap_id_t,
                "table_id": table_id_t,
                "prompt": messages_t,
                "gt": gt_t
            })

            prompts_without_system_prompt_list.append({
                "qap_id": qap_id_t,
                "table_id": table_id_t,
                "prompt": messages_t_without_system,
                "gt": gt_t
            })

    ### Write as jsonl
    with open(output_file_name, "w") as f:
        for x in prompts_list:
            f.write(json.dumps(x) + "\n")

    with open(output_file_name.replace("jsonl", "--without_system_prompts.jsonl"), "w") as f:
        for x in prompts_without_system_prompt_list:
            f.write(json.dumps(x) + "\n")

    return True

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Make prompts file from QAPs file')
    parser.add_argument('--input_file', type=str, help='Input QAPs file')
    parser.add_argument('--output_file', type=str, help='Output file name')

    args = parser.parse_args()

    make_prompts_file_from_qaps_file(args.input_file, args.output_file)




