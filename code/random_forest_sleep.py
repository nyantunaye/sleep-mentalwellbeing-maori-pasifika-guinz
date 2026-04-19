# =============================================================================
# Script: random_forest_sleep.py
# Project: Sleep Patterns and Mental Wellbeing in Māori and Pasifika Youth
# Data: Growing Up in New Zealand (GUiNZ) Study
# Author: Nyan Tun Aye | AUT Master of Analytics
#
# Python equivalent of 04_random_forest.R
# Predicts sleep duration and sleep quality using Random Forest models
# =============================================================================

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.model_selection import cross_val_score, KFold
from sklearn.preprocessing import LabelEncoder
from sklearn.inspection import permutation_importance
from sklearn.metrics import mean_squared_error, classification_report
import warnings
warnings.filterwarnings("ignore")

# =============================================================================
# NOTE: GUiNZ data is accessed under a data access agreement.
# Raw data files are NOT included in this repository.
# Replace the load_data() section with your actual data path.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

def load_data(filepath: str) -> pd.DataFrame:
    """
    Load the cleaned age-12 dataset.
    Replace filepath with your actual data path.
    """
    df = pd.read_csv(filepath)
    return df


# -----------------------------------------------------------------------------
# 2. PREPARE FEATURES
# -----------------------------------------------------------------------------

FEATURE_COLS = [
    "ethnicity",
    "NZDEP2018_5_Y12C",
    "RURALITY_BIN_UR2018_Y12C",
    "TRANS_NB_CAT_Y12C",
    "DEPRESS_SCORE_10_Y12C",
    "PAS_T_SCORE2_Y12C",
    "CDRISC_SUM_Y12C"
]

FEATURE_LABELS = [
    "Ethnicity",
    "NZ Deprivation Index",
    "Rural vs Urban",
    "Trans/NB vs Boy",
    "Depression",
    "Anxiety",
    "Resilience"
]


def prepare_features(df: pd.DataFrame, outcome_col: str) -> tuple:
    """
    Encode categorical variables and return X, y arrays.
    """
    data = df[FEATURE_COLS + [outcome_col]].dropna().copy()

    # Encode categorical columns
    le = LabelEncoder()
    for col in ["ethnicity", "TRANS_NB_CAT_Y12C"]:
        if col in data.columns:
            data[col] = le.fit_transform(data[col].astype(str))

    X = data[FEATURE_COLS].values
    y = data[outcome_col].values

    return X, y


# -----------------------------------------------------------------------------
# 3. PLOT FEATURE IMPORTANCE
# -----------------------------------------------------------------------------

def plot_feature_importance(importances: np.ndarray,
                            title: str,
                            save_path: str = None):
    """
    Horizontal dot plot of feature importances — styled like the R varImpPlot.
    """
    indices = np.argsort(importances)
    labels  = [FEATURE_LABELS[i] for i in indices]
    values  = importances[indices]

    fig, ax = plt.subplots(figsize=(8, 5))
    ax.hlines(range(len(labels)), 0, values,
              color="steelblue", linewidth=1, linestyle="--")
    ax.plot(values, range(len(labels)),
            "o", color="steelblue", markersize=8)
    ax.set_yticks(range(len(labels)))
    ax.set_yticklabels(labels)
    ax.set_xlabel("Mean Decrease in Impurity (Feature Importance)")
    ax.set_title(title)
    ax.grid(axis="x", linestyle="--", alpha=0.4)
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches="tight")
        print(f"  Plot saved → {save_path}")
    plt.show()


# -----------------------------------------------------------------------------
# 4. RANDOM FOREST — SLEEP DURATION (REGRESSION)
# Figure 7 equivalent
# -----------------------------------------------------------------------------

def run_rf_sleep_duration(df: pd.DataFrame):
    print("\n" + "="*60)
    print("Random Forest: Predicting Sleep Duration (Age 12)")
    print("="*60)

    X, y = prepare_features(df, outcome_col="total_sleep_age12")

    # Train model
    rf = RandomForestRegressor(
        n_estimators=500,
        max_features=3,
        random_state=42,
        n_jobs=-1
    )
    rf.fit(X, y)

    # OOB score (need oob_score=True)
    rf_oob = RandomForestRegressor(
        n_estimators=500,
        max_features=3,
        oob_score=True,
        random_state=42,
        n_jobs=-1
    )
    rf_oob.fit(X, y)
    oob_mse = mean_squared_error(y, rf_oob.oob_prediction_)
    print(f"\nOOB Mean Squared Residual : {oob_mse:.4f}")
    print(f"OOB R² (explained var)    : {rf_oob.oob_score_:.4f}")

    # Feature importance
    importances = rf.feature_importances_
    print("\nVariable Importance (Mean Decrease Impurity):")
    for label, imp in sorted(zip(FEATURE_LABELS, importances),
                              key=lambda x: -x[1]):
        print(f"  {label:<25} {imp:.6f}")

    plot_feature_importance(
        importances,
        title="Random Forest Variable Importance\nfor Sleep Duration (Age 12)",
        save_path="outputs/figures/fig07_rf_sleep_duration_py.png"
    )

    # 10-fold cross-validation
    print("\n--- 10-Fold Cross-Validation ---")
    cv = KFold(n_splits=10, shuffle=True, random_state=42)
    cv_scores = cross_val_score(rf, X, y, cv=cv, scoring="r2")
    print(f"CV R² scores : {cv_scores.round(4)}")
    print(f"Mean CV R²   : {cv_scores.mean():.4f} ± {cv_scores.std():.4f}")

    return rf


# -----------------------------------------------------------------------------
# 5. RANDOM FOREST — MOTHER-REPORTED SLEEP QUALITY (CLASSIFICATION)
# Figure 16 equivalent
# -----------------------------------------------------------------------------

def run_rf_mother_sleep_quality(df: pd.DataFrame):
    print("\n" + "="*60)
    print("Random Forest: Mother-Reported Sleep Quality (Age 12)")
    print("="*60)

    X, y = prepare_features(df, outcome_col="MotherSleepQ")

    rf = RandomForestClassifier(
        n_estimators=500,
        max_features=3,
        oob_score=True,
        random_state=42,
        n_jobs=-1
    )
    rf.fit(X, y)

    oob_error = 1 - rf.oob_score_
    print(f"\nOOB Error Rate : {oob_error * 100:.2f}%")
    print(f"OOB Accuracy   : {rf.oob_score_ * 100:.2f}%")

    # Feature importance
    importances = rf.feature_importances_
    print("\nVariable Importance (Mean Decrease Impurity):")
    for label, imp in sorted(zip(FEATURE_LABELS, importances),
                              key=lambda x: -x[1]):
        print(f"  {label:<25} {imp:.6f}")

    plot_feature_importance(
        importances,
        title="Random Forest Variable Importance\nfor Mother-Reported Sleep Quality (Age 12)",
        save_path="outputs/figures/fig16_rf_mother_sleep_py.png"
    )

    print("\nClassification Report:")
    y_pred = rf.predict(X)
    print(classification_report(y, y_pred))

    return rf


# -----------------------------------------------------------------------------
# 6. RANDOM FOREST — CHILD-REPORTED SLEEP QUALITY (CLASSIFICATION)
# Figure 18 equivalent
# -----------------------------------------------------------------------------

def run_rf_child_sleep_quality(df: pd.DataFrame):
    print("\n" + "="*60)
    print("Random Forest: Child-Reported Sleep Quality (Age 12)")
    print("="*60)

    X, y = prepare_features(df, outcome_col="ChildSleepQ")

    rf = RandomForestClassifier(
        n_estimators=500,
        max_features=3,
        oob_score=True,
        random_state=42,
        n_jobs=-1
    )
    rf.fit(X, y)

    oob_error = 1 - rf.oob_score_
    print(f"\nOOB Error Rate : {oob_error * 100:.2f}%")
    print(f"OOB Accuracy   : {rf.oob_score_ * 100:.2f}%")

    # Feature importance
    importances = rf.feature_importances_
    print("\nVariable Importance (Mean Decrease Impurity):")
    for label, imp in sorted(zip(FEATURE_LABELS, importances),
                              key=lambda x: -x[1]):
        print(f"  {label:<25} {imp:.6f}")

    plot_feature_importance(
        importances,
        title="Random Forest Variable Importance\nfor Child-Reported Sleep Quality (Age 12)",
        save_path="outputs/figures/fig18_rf_child_sleep_py.png"
    )

    print("\nClassification Report:")
    y_pred = rf.predict(X)
    print(classification_report(y, y_pred))

    return rf


# -----------------------------------------------------------------------------
# 7. MAIN — RUN ALL THREE MODELS
# -----------------------------------------------------------------------------

if __name__ == "__main__":
    import os

    # Create output directory
    os.makedirs("outputs/figures", exist_ok=True)

    # -------------------------------------------------------------------------
    # Load data — replace with your actual file path
    # -------------------------------------------------------------------------
    # df = load_data("data/processed/data_clean.csv")

    # -------------------------------------------------------------------------
    # Run models
    # -------------------------------------------------------------------------
    # rf_duration = run_rf_sleep_duration(df)
    # rf_mother   = run_rf_mother_sleep_quality(df)
    # rf_child    = run_rf_child_sleep_quality(df)

    print("\nScript complete.")
    print("Uncomment the load_data() and model calls above to run the analysis.")
    print("\nRequired packages:")
    print("  pip install pandas numpy matplotlib scikit-learn")
