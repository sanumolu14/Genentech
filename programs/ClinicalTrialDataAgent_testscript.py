# Test Script ------------------------------------------------

import pandas as pd
from clinical_trial_data_agent import ClinicalTrialDataAgent

ae = pd.read_csv("adae.csv")

agent = ClinicalTrialDataAgent(ae_df=ae, use_mock_llm=True)

test_questions = [
    "Give me the subjects who had adverse events of Moderate severity.",
    "Which subjects had Headache?",
    "Show me subjects with Cardiac adverse events."
]

for question in test_questions:
    print("=" * 50)
    print(question)
    result = agent.ask(question)
    print(result)