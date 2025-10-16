# Reinforcement-learning-and-conformal-prediction-Neural-Fitted-Q-Iteration-in-dynamic-systems
During my master thesis, I've studied reinforcement learning with a specific focus in Q learning and Fitted Q-Iteration, using these methods in order to manage the optimal functioning of a water tank in the city of Milan. To quantify the uncertainty in the estimates of q-values, I've applied conformal prediction for non-excheangeble data.
# Reinforcement Learning, Fitted Q Iteration, and Conformal Prediction  
### Application to the Control of an Urban Water Tank

In recent decades, there has been a rapid development of *machine learning* and artificial intelligence, which has led to their application in a wide variety of contexts and problems of varying complexity, such as *clustering*, classification, and prediction.  
Recently, however, interest has grown in methods capable of handling more complex problems, and in particular, within the field of dynamic systems, the need has arisen to develop algorithms capable of making **sequential decisions**, where traditional *supervised* and *unsupervised learning* algorithms are not effective.  

Dynamic systems are systems that have a continuous influence on themselves, in which at each time step it is necessary to make a decision that affects the system’s state at the next time step.  
In this context, **Reinforcement Learning (RL)**, introduced in [Sutton & Barto, 1998], has established itself as one of the most promising paradigms.  

*Reinforcement Learning* is a branch of *machine learning* concerned with modeling the interaction between a decision-making subject, called the **agent**, and the **environment**, with the goal of learning optimal decision-making strategies through direct experience.  
The main objective of an RL algorithm is to identify a sequence of decision rules, called a **policy**, that is optimal with respect to a given objective.  
Unlike *supervised* or *unsupervised learning*, RL does not rely on static data but on **sequential data**, where the phenomenon evolves over time and it is possible to observe both the evolution of the agent’s behavior and the progress of its interaction with the environment.  
Learning takes place through a **trial-and-error** process, in which at each time step an action is taken based on a numerical signal called **reward**, which measures the immediate quality of the decision made.

---

A fundamental contribution to the development of RL algorithms is represented by **Q-learning**, a simple yet effective method proposed by [Watkins, 1992] for determining an optimal sequence of decisions by considering the long-term effects of the implemented *policy* through the **Q-function**, and not only the immediate reward.  

However, in its simplest version, Q-learning has limited usefulness, as it is suitable only for scenarios with small state and action spaces, showing limitations in scalability and generalization.  
To overcome these issues, several approaches have been proposed that combine **Q-learning** with **regression methods** for estimating the Q-function.  
Among them, **Fitted Q Iteration (FQI)** [Ernst et al., 2005] represents one of the most versatile and effective solutions.  

To make FQI even more flexible, it is possible to use **artificial neural networks** as regression models for Q-function estimation, obtaining **Neural Fitted Q Iteration (NFQ)**, which is capable of learning complex nonlinear relationships and generalizing even in large state–action spaces.

---

A crucial aspect in the development of *reinforcement learning* algorithms concerns the ability not only to provide point estimates of the quantities of interest but also to **quantify the uncertainty** associated with the decisions.  
In complex, high-impact application domains — such as robotics, medicine, and finance — having a plausible range of values, rather than a single prediction, is far more informative and useful for decision-making.  

In this context, **Conformal Prediction (CP)**, first introduced in [Vovk et al., 2005], provides a *distribution-free* methodology based on the assumption of exchangeability, which allows the construction of predictive intervals with valid coverage guarantees, regardless of the learning model adopted.  

In the context of RL — and particularly within the FQI algorithm — the exchangeability assumption may be violated, since the data used for prediction may come from generative processes different from those used for training.  
To address this issue, recent research has focused on integrating CP with RL algorithms, aiming to extend its validity to **sequential scenarios**.  
Some of these extensions, proposed for example in [Zhang et al., 2023] and [Barber et al., 2023], have been analyzed in this thesis.

---

The primary goal of this thesis is to analyze the **theoretical and computational foundations** of RL and FQI algorithms, evaluating their performance within a real dynamic system — the control of a water tank in an urban setting.  
A secondary goal is to explore the use of **Conformal Prediction** as a tool for quantifying uncertainty within the employed RL methodologies.

