---
name: write-skill
description: Provides the complete specification and guidelines for creating new Agent Skills. Use when you need to create or modify skills in the .kilocode/skills/ directory.
---

# Agent Skills Specification

This skill contains the complete format specification for creating new Agent Skills, based on the official Agent Skills documentation.

## Directory Structure

A skill is a directory containing at minimum a `SKILL.md` file:

```
skill-name/
└── SKILL.md          # Required
```

You can optionally include additional directories such as `scripts/`, `references/`, and `assets/` to support your skill.

## SKILL.md Format

The `SKILL.md` file must contain YAML frontmatter followed by Markdown content.

### Frontmatter (Required)

```yaml
---
name: skill-name
description: A description of what this skill does and when to use it.
---
```

With optional fields:

```yaml
---
name: pdf-processing
description: Extract text and tables from PDF files, fill forms, merge documents.
license: Apache-2.0
metadata:
  author: example-org
  version: "1.0"
---
```

| Field           | Required | Constraints                                                                                                       |
| --------------- | -------- | ----------------------------------------------------------------------------------------------------------------- |
| `name`          | Yes      | Max 64 characters. Lowercase letters, numbers, and hyphens only. Must not start or end with a hyphen.             |
| `description`   | Yes      | Max 1024 characters. Non-empty. Describes what the skill does and when to use it.                                 |
| `license`       | No       | License name or reference to a bundled license file.                                                              |
| `compatibility` | No       | Max 500 characters. Indicates environment requirements (intended product, system packages, network access, etc.). |
| `metadata`      | No       | Arbitrary key-value mapping for additional metadata.                                                              |
| `allowed-tools` | No       | Space-delimited list of pre-approved tools the skill may use. (Experimental)                                      |

#### `name` Field

- Must be 1-64 characters
- May only contain unicode lowercase alphanumeric characters and hyphens (`a-z` and `-`)
- Must not start or end with `-`
- Must not contain consecutive hyphens (`--`)
- Must match the parent directory name

Valid examples: `pdf-processing`, `data-analysis`, `code-review`

Invalid examples: `PDF-Processing` (uppercase), `-pdf` (starts with hyphen), `pdf--processing` (consecutive hyphens)

#### `description` Field

- Must be 1-1024 characters
- Should describe both what the skill does and when to use it
- Should include specific keywords that help agents identify relevant tasks

Good example: "Extracts text and tables from PDF files, fills PDF forms, and merges multiple PDFs. Use when working with PDF documents or when the user mentions PDFs, forms, or document extraction."

#### Other Fields

- `license`: Specifies the license (e.g., "Apache-2.0" or "Proprietary. LICENSE.txt has complete terms")
- `compatibility`: Environment requirements (e.g., "Requires git, docker, jq, and access to the internet")
- `metadata`: Additional properties (e.g., author, version)
- `allowed-tools`: Pre-approved tools (experimental)

### Body Content

The Markdown body after the frontmatter contains the skill instructions. Write whatever helps agents perform the task effectively.

Recommended sections:
- Step-by-step instructions
- Examples of inputs and outputs
- Common edge cases

Keep the main `SKILL.md` under 500 lines. Move detailed reference material to separate files.

## Optional Directories

### scripts/

Contains executable code that agents can run. Scripts should be self-contained, include helpful error messages, and handle edge cases.

### references/

Contains additional documentation:
- `REFERENCE.md` - Detailed technical reference
- Domain-specific files

### assets/

Contains static resources:
- Templates
- Images
- Data files

## Progressive Disclosure

Skills should be structured efficiently:
1. Metadata (~100 tokens): `name` and `description` loaded at startup
2. Instructions (< 5000 tokens recommended): Full `SKILL.md` body loaded when activated
3. Resources (as needed): Files loaded on demand

## File References

Use relative paths from the skill root:
- `references/REFERENCE.md`
- `scripts/extract.py`

Keep references one level deep.

## Validation

Use the skills-ref tool to validate:
```bash
skills-ref validate ./my-skill
```

## Step-by-Step Guide to Writing a New Skill

1. **Choose a name**: Follow the naming rules. The directory name must match the `name` field.

2. **Write the description**: Make it clear what the skill does and when to use it. Include keywords for discoverability.

3. **Create the directory**: `mkdir .kilocode/skills/your-skill-name`

4. **Create SKILL.md**: Add frontmatter with required fields, then detailed instructions in Markdown.

5. **Add optional content**: If needed, add scripts/, references/, assets/ subdirectories.

6. **Validate**: Run `skills-ref validate` if available.

7. **Test**: Ensure the skill works as intended in your agent environment.

Remember: Skills are loaded progressively, so keep the main file concise and reference additional files as needed.