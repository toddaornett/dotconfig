(after! ellama
  (require 'llm-ollama)
  (setq ellama-provider
    (make-llm-ollama
      :host (or (getenv "LLM_OLLAMA_HOST") "localhost")
      :port (or (when-let ((p (getenv "LLM_OLLAMA_PORT"))) (string-to-number p)) 11434)
      :chat-model (or (getenv "LLM_OLLAMA_MODEL") "qwen3.6:latest")
      :embedding-model (or (getenv "LLM_OLLAMA_EMBEDDING_MODEL") "nomic-embed-text:latest")))
  (setq ellama-srt-enabled t)
  (ellama-setup-agentic-coding)
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1))

(setq llm-warn-on-nonfree nil)

(map! :leader
  (:prefix ("l" . "llm")
    "c" #'ellama-chat
    "a" #'ellama-ask-selection
    "d" #'ellama-context-add-directory
    "f" #'ellama-context-add-file
    "p" #'ellama-plan-and-act
    "s" #'ellama-summarize
    "r" #'ellama-rewrite
    "L" #'ellama-load-session
    "R" #'ellama-rename-session
    "k" #'ellama-session-kill
    "w" #'ellama-session-switch))
