export type Requirement = {
  id: string;
  title: string;
  description: string;
  rationale?: string | null;
  acceptance_criteria: string[];
  priority: string;
};

export type ClarificationQuestion = {
  id: string;
  text: string;
  options?: string[] | null;
  expected_impact: string;
  axis?: string | null;
  priority?: number | null;
  expected_score_gain?: number | null;
  targets?: string[] | null;
};

export type RequirementsBundle = {
  prompt_id: string;
  project_id: string;
  business_requirements: Requirement[];
  functional_requirements: Requirement[];
  assumptions: string[];
  risks: string[];
  score: number;
  clarification_questions?: ClarificationQuestion[] | null;
};



