export type SimilarMatch = {
  kind: "epic" | "issue";
  id: string;
  iid: string;
  title: string;
  project_id: string;
  status?: string | null;
  similarity?: number | null;
  url?: string | null;
  link_decision?: "accepted" | "rejected" | null;
};

export type BacklogTask = {
  id: string;
  title: string;
  description: string;
  acceptance_criteria: string[];
  dependencies: string[];
  similar?: SimilarMatch[] | null;
  // UI-only routing override
  target_project_id?: string | null;
};

export type BacklogEpic = {
  id: string;
  title: string;
  description: string;
  tasks: BacklogTask[];
  similar?: SimilarMatch[] | null;
  // UI-only routing override
  target_project_id?: string | null;
};

export type GeneratedBacklogBundle = {
  prompt_id: string;
  project_id: string;
  epics: BacklogEpic[];
  assumptions: string[];
  risks: string[];
  score: number;
  markdown_text?: string | null;
};

export type EnhancedTask = {
  item_id: string;
  title: string;
  description: string;
  acceptance_criteria: string[];
  dependencies?: string[] | null;
};



