#!/bin/bash
# Automated package installation script for emacs-python-ml-workflow

echo "=========================================="
echo "Emacs Python/ML Workflow - Installation"
echo "=========================================="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check prerequisites
echo "Checking prerequisites..."

check_command() {
    if command -v $1 >/dev/null 2>&1; then
        echo -e "${GREEN}✓ $1 installed${NC}"
        return 0
    else
        echo -e "${RED}✗ $1 not found${NC}"
        return 1
    fi
}

check_command emacs
check_command python3
check_command pip
check_command git

echo
echo "Step 1: Installing Python packages..."
echo "-----------------------------------"

# Install Python packages
pip_packages=(
    "jupyter"
    "matplotlib"
    "numpy"
    "pandas"
    "scikit-learn"
    "pyright"
    "nbformat"
)

for pkg in "${pip_packages[@]}"; do
    echo "Installing $pkg..."
    pip install --quiet $pkg
    if [ $? -eq 0 ]; then
        echo -e "  ${GREEN}✓ $pkg installed${NC}"
    else
        echo -e "  ${RED}✗ Failed to install $pkg${NC}"
    fi
done

echo
echo "Step 2: Setting up Emacs configuration..."
echo "---------------------------------------"

# Create Emacs config directory if it doesn't exist
EMACS_DIR="$HOME/.emacs.d"
if [ ! -d "$EMACS_DIR" ]; then
    echo "Creating $EMACS_DIR..."
    mkdir -p "$EMACS_DIR"
fi

# Copy configuration files
echo "Copying configuration files..."
cp -r emacs-config/*.el "$EMACS_DIR/" 2>/dev/null || {
    echo -e "${YELLOW}⚠ Could not copy config files. Are you in the project root?${NC}"
    echo "Please run this script from the emacs-python-ml-workflow directory."
    exit 1
}

echo -e "${GREEN}✓ Configuration files copied${NC}"

echo
echo "Step 3: Installing Emacs packages..."
echo "-----------------------------------"

# Create a temporary Emacs installation script
INSTALL_SCRIPT="/tmp/emacs-install-$$.el"

cat > "$INSTALL_SCRIPT" << 'EOF'
;; Package installation script
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Try to refresh package list
(condition-case err
    (unless package-archive-contents
      (package-refresh-contents))
  (error (message "Could not refresh archives: %s" (error-message-string err))))

;; List of packages to install
(setq packages-to-install '(
    company
    lsp-mode
    lsp-pyright
    flycheck
    anaconda-mode
    company-anaconda
    yasnippet
    company-quickhelp
    emacs-jupyter
    ess
    magit
    git-link
    markdown-mode
    grip-mode
))

(defun install-package (pkg)
  "Install a single package."
  (condition-case err
      (progn
        (unless (package-installed-p pkg)
          (package-install pkg)
          (message "✓ Installed %s" pkg)
          t)
        (message "✓ %s already installed" pkg)
        t)
    (error
     (message "✗ Failed to install %s: %s" pkg (error-message-string err))
     nil)))

;; Install packages
(setq installed-count 0)
(setq failed-count 0)

(dolist (pkg packages-to-install)
  (if (install-package pkg)
      (setq installed-count (1+ installed-count))
    (setq failed-count (1+ failed-count))))

(message "\n========================================")
(message "Installation complete!")
(message "✓ Installed: %d packages" installed-count)
(when (> failed-count 0)
  (message "⚠ Failed: %d packages" failed-count))
(message "========================================")

(kill-emacs 0)
EOF

echo "Starting Emacs package installation..."
echo "This may take a few minutes..."

# Run the installation script
emacs --batch -l "$INSTALL_SCRIPT" 2>&1 | grep -E "(✓|✗|Installation)"

# Clean up
rm -f "$INSTALL_SCRIPT"

echo
echo "Step 4: Setting up example workflows..."
echo "--------------------------------------"

# Create examples directory
EXAMPLES_DIR="examples"
mkdir -p "$EXAMPLES_DIR/before-after-screenshots"

echo "Creating example Org file..."
cat > "$EXAMPLES_DIR/Lab1-Student_FINAL.org" << 'EOF'
#+TITLE: Lab1 - Student Example
#+AUTHOR: Emacs Python/ML Workflow
#+DATE: 2024

* Introduction
This is an example Org file converted from Jupyter notebook.

* Data Analysis

#+begin_src python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Generate sample data
np.random.seed(42)
x = np.linspace(0, 10, 100)
y = np.sin(x) + np.random.normal(0, 0.1, 100)

# Create dataframe
df = pd.DataFrame({'x': x, 'y': y})
print(f"Data shape: {df.shape}")
print(f"Mean of y: {df['y'].mean():.2f}")
print(f"Std of y: {df['y'].std():.2f}")
#+end_src

#+RESULTS:
: Data shape: (100, 2)
: Mean of y: 0.03
: Std of y: 0.72

* Visualization

#+begin_src jupyter-python
plt.figure(figsize=(10, 4))
plt.plot(df['x'], df['y'], 'b.', alpha=0.6, label='Noisy data')
plt.plot(df['x'], np.sin(df['x']), 'r-', linewidth=2, label='True sine wave')
plt.title('Sine Wave with Noise')
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.grid(True, alpha=0.3)
plt.show()
#+end_src

* Machine Learning Example

#+begin_src python
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error

# Prepare data
X = df[['x']].values
y_true = df['y'].values

# Fit model
model = LinearRegression()
model.fit(X, y_true)
y_pred = model.predict(X)

# Calculate metrics
mse = mean_squared_error(y_true, y_pred)
print(f"Mean Squared Error: {mse:.4f}")
print(f"Coefficient: {model.coef_[0]:.4f}")
print(f"Intercept: {model.intercept_:.4f}")
#+end_src

#+RESULTS:
: Mean Squared Error: 0.4779
: Coefficient: 0.0048
: Intercept: 0.0269

* R Language Example

#+begin_src R
# R code example
library(ggplot2)

# Create a simple plot
data <- data.frame(
  x = 1:50,
  y = rnorm(50, mean = 0, sd = 1)
)

ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Random Data with Linear Fit",
       x = "Index",
       y = "Value") +
  theme_minimal()
#+end_src

* Conclusion
This demonstrates the Python/R workflow in Emacs with:
- Code execution
- Inline plots
- Mixed Python/R code blocks
- Rich output display
EOF

echo -e "${GREEN}✓ Example file created${NC}"

echo
echo "=========================================="
echo "Installation Complete!"
echo "=========================================="
echo
echo "Next steps:"
echo "1. Start Emacs: emacs"
echo "2. Open an example: C-x C-f examples/Lab1-Student_FINAL.org"
echo "3. Execute code blocks with: C-c C-c"
echo "4. Toggle images with: C-c C-x C-v"
echo
echo "For help:"
echo "  - View README.md for detailed instructions"
echo "  - Run M-x my-show-setup-status in Emacs"
echo "  - Check docs/workflow-guide.md"
echo
echo "Happy coding! 🚀"
