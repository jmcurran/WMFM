# What's My Fitted Model? (WMFM)

WMFM is an R/Shiny app for fitting statistical models and generating plain-language explanations of fitted models for teaching and learning.

## Quick start

Install and load WMFM, then start the teaching app from R:

```r
library(WMFM)
runWMFM()
```

The app guides students through choosing data, fitting a model, viewing fitted equations, and generating a plain-language explanation. Many model-fitting, plotting, and classroom-navigation features can be explored without contacting an external AI service.

AI-generated explanations require a provider chosen and configured by the user. WMFM does not contact commercial AI APIs or a local Ollama server during package checks, package loading, or ordinary non-AI workflows.

## Teaching workflow

A typical classroom workflow is:

1. Load or select an example data set.
2. State the research question in the model tab.
3. Fit a model appropriate to the response variable and explanatory variables.
4. Review fitted equations, confidence intervals, diagnostics, and model plots.
5. Generate a student-facing explanation once an AI provider has been configured.
6. Ask supported follow-up questions when a deterministic answer is available.

The app is intended to help students connect fitted models to interpretation. It is not intended to hide the fitted model, the response scale, or the assumptions behind the explanation.

## Configuring an AI provider

WMFM needs access to an AI provider before it can generate fitted-model explanations. Configure either a commercial provider or a local Ollama model before using AI-generated explanation features.

Provider access is opt-in. API keys are read from environment variables. Set these variables in `~/.Renviron`, then restart R before starting WMFM. The app does not ask for API keys in the UI and does not look for them in the WMFM config file.

WMFM never stores API keys in its config file.

### Commercial providers require API credits

Claude and OpenAI access require provider API credentials with API billing or API credits enabled. A normal Claude subscription or ChatGPT Plus subscription does not automatically provide API access for an R package.

### Claude / Anthropic

Add your Anthropic API key to `~/.Renviron`:

```text
ANTHROPIC_API_KEY=your_key_here
```

Restart R after editing `~/.Renviron`. Then start WMFM, open **Settings**, choose **Claude / Anthropic**, click **Apply provider**, and click **Save provider config** if you want Claude remembered for future sessions.

### OpenAI

Add your OpenAI API key to `~/.Renviron`:

```text
OPENAI_API_KEY=your_key_here
```

Restart R after editing `~/.Renviron`. Then start WMFM and select the OpenAI provider in **Settings** when that provider is enabled by the app.

### Local Ollama

To use a local model, install and start Ollama, then pull at least one model, for example:

```bash
ollama pull llama3.1
```

In WMFM, open **Settings**, choose **Ollama (local)**, set the Ollama base URL, usually:

```text
http://localhost:11434
```

Choose the model, click **Apply provider**, and click **Save provider config** if you want the local provider remembered for future sessions.

Many users will have only one local Ollama model installed. That is fine: select that model and save the provider config.

WMFM only attempts Ollama model discovery when Ollama is the selected provider and a local Ollama configuration is available.

### First run with no provider configured

If WMFM starts without a configured commercial-provider API key and without saved Ollama settings, it displays a setup message and directs you back to this README section. Configure a provider through `~/.Renviron` or through the saved local Ollama settings before using AI-generated explanations.

## CRAN and offline checks

WMFM's package checks and examples are designed not to require API credentials, live AI services, or a running Ollama server. Tests should use deterministic helpers, mocks, or fake providers rather than real LLM calls.

Development and CRAN checks may validate package metadata, examples, and offline test behavior, but they should not attempt to discover Ollama models or contact commercial provider APIs unless a user explicitly starts a provider action in the app or calls a provider function directly.
