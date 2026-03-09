# REPLICATION-ON-COMPUSORY-LICENCING

🧪 The Great Patent Heist: Boosting US Innovation
Replicating Moser & Voena (2012): Compulsory Licensing & The Trading with the Enemy Act

Imagine it’s 1917. World War I is raging, and the US realizes it's totally dependent on German chemical giants (Bayer, BASF) for everything from dyes to drugs. The solution? The Trading with the Enemy Act (TWEA). Uncle Sam confiscated over 4,500 German patents and handed the keys to American firms.

This repository replicates the famous study by Moser and Voena to see if this "intellectual property expropriation" actually helped or hindered domestic innovation.
🧐 The Big Question

Does forcing foreign inventors to share their secrets discourage local R&D, or does it provide the "spark" needed for a domestic boom?.
🛠️ The Scientific Toolkit

To rebuild this historical analysis, we used:

    The Brain: Stata (originally written for Stata 10).

    The Data: A massive haul of 129,943 chemical patents from the USPTO (1875-1939) and historical records from the Chemical Foundation.

    The Secret Sauce: Difference-in-Differences (DiD) models to compare "treated" technologies (those that got German licenses) against the control group.

📂 Inside the Lab (Repository Map)

Everything you need to spark a chemical revolution is listed here:

    code_for_replication.do: The master script. Run this to generate every table and graph.

    chem_patents_maindataset.dta: The core database of American chemical ingenuity.

    dupont_data.dta: Special case study data on Du Pont—the ultimate "learner-by-doing".

    fig1.dta, fig5.dta, fig10.dta: Data for the key visual proofs.

    table1.dta to table8.dta: Datasets for the main regression results.

📈 The Verdict: Victory!

Spoiler alert: The US won.

    20% Boost: Domestic inventors produced significantly more patents in the subclasses that were "unlocked" by the TWEA.

    Learning by Doing: It wasn't just about stealing secrets; it was about US firms finally having the time to learn complex "tacit knowledge".

    The Du Pont Effect: Firms holding their own licenses saw a 4x larger effect than those just watching from the sidelines.

🚀 How to Replicate

    Boot up Stata.

    Load the mission: Run code_for_replication.do.

    Witness the data: Watch as Figure 1 (the collapse and recovery of German patents) and Figure 6 (the annual treatment effects) come back to life.
