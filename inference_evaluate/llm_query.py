"""
Class for querying LLM inference
"""
import os
import torch
from vllm import LLM, SamplingParams

import json
import re

class llm_query:
    def __init__(self, model_path, sys_prompt=None, gpu_memory_utilization=0.99, tensor_parallel_size=1, **kwargs):
        # Path to local copy of model
        self.model_path = model_path
        
        # System prompt to be used for all queries
        self.sys_prompt = sys_prompt
        
        # Load VLLM Model Instance
        self.llm = LLM(model=model_path, gpu_memory_utilization=gpu_memory_utilization, 
            tensor_parallel_size=tensor_parallel_size, trust_remote_code=True, **kwargs)
        
        # Load Tokenizer
        self.tokenizer = self.llm.get_tokenizer()

        # Set chat template if not already set
        if self.tokenizer.chat_template == None:
            self.tokenizer.chat_template = "{% if not add_generation_prompt is defined %}{% set add_generation_prompt = false %}{% endif %}{% for message in messages %}{{'<|im_start|>' + message['role'] + '\n' + message['content'] + '<|im_end|>' + '\n'}}{% endfor %}{% if add_generation_prompt %}{{ '<|im_start|>assistant\n' }}{% endif %}"

    
    def query(self, user_prompts, system_prompts=None, temperature=0.1, repetition_penalty=1.1, max_tokens=128000):
        sampling_params = SamplingParams(temperature=temperature, repetition_penalty=repetition_penalty, max_tokens=max_tokens)
        model_output = self.llm.chat(user_prompts, sampling_params)
            
        return model_output

