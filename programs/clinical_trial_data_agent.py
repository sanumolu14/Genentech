# clinical_trial_data_agent.py

import json
import re
import pandas as pd


class ClinicalTrialDataAgent:
    def __init__(self, ae_df, use_mock_llm=True):
        self.ae_df = ae_df.copy()
        self.use_mock_llm = use_mock_llm

        self.schema_description = """
        AE Dataset Schema:
        - USUBJID: Unique subject ID
        - AESEV: Adverse event severity/intensity. Examples: MILD, MODERATE, SEVERE
        - AETERM: Adverse event reported term. Examples: Headache, Nausea
        - AESOC: Body system / system organ class. Examples: Cardiac disorders, Skin disorders
        """

    def build_prompt(self, question):
        return f"""
        You are a clinical safety data assistant.

        Convert the user's question into JSON only.

        {self.schema_description}

        Rules:
        - If question asks about severity or intensity, use AESEV.
        - If question asks about a specific adverse event/condition, use AETERM.
        - If question asks about body system/system organ class, use AESOC.

        Return JSON with:
        {{
          "target_column": "AESEV or AETERM or AESOC",
          "filter_value": "value to search"
        }}

        User question: {question}
        """

    def mock_llm_parse(self, question):
        q = question.lower()

        if "moderate" in q:
            return {"target_column": "AESEV", "filter_value": "MODERATE"}
        if "mild" in q:
            return {"target_column": "AESEV", "filter_value": "MILD"}
        if "severe" in q:
            return {"target_column": "AESEV", "filter_value": "SEVERE"}

        if "headache" in q:
            return {"target_column": "AETERM", "filter_value": "HEADACHE"}
        if "nausea" in q:
            return {"target_column": "AETERM", "filter_value": "NAUSEA"}

        if "cardiac" in q or "heart" in q:
            return {"target_column": "AESOC", "filter_value": "CARDIAC"}
        if "skin" in q:
            return {"target_column": "AESOC", "filter_value": "SKIN"}

        return {"target_column": "AETERM", "filter_value": question}

    def parse_question(self, question):
        prompt = self.build_prompt(question)
        print("\n--- Prompt Sent to LLM ---")
        print(prompt)

        if self.use_mock_llm:
            parsed = self.mock_llm_parse(question)
        else:
            raise NotImplementedError(
                "Connect OpenAI/LangChain here if API key is available."
            )

        print("\n--- Parsed JSON Output ---")
        print(json.dumps(parsed, indent=2))
        return parsed

    def execute_query(self, parsed_output):
        target_column = parsed_output["target_column"]
        filter_value = parsed_output["filter_value"]

        if target_column not in self.ae_df.columns:
            raise ValueError(f"{target_column} not found in AE dataset")

        filtered = self.ae_df[
            self.ae_df[target_column]
            .astype(str)
            .str.upper()
            .str.contains(str(filter_value).upper(), na=False)
        ]

        subject_ids = sorted(filtered["USUBJID"].dropna().unique().tolist())

        return {
            "target_column": target_column,
            "filter_value": filter_value,
            "unique_subject_count": len(subject_ids),
            "matching_subject_ids": subject_ids
        }

    def ask(self, question):
        parsed_output = self.parse_question(question)
        return self.execute_query(parsed_output)


