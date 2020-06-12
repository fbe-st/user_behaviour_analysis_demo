# User Behaviour Analysis Demo

## DESCRIPTION
We want to deliver the results from our different analysis on User Behaviour, in an interactive and fun way for our internal stakeholders and audience. For this reason we developed a Shiny dashboard which allows to slice results by clients, selectively aggregate groups of clients, or remove specific clients which might bias/distort the results. Additionally, for one of the time-line analysis the range of the time line can also be modified in order to focus the analysis from a few days to up to six months.

## RESULTS & INSIGHTS
The current demo includes the following analyses:

### Action Counts 

- Analyses "how many" users executed at least once one of the following actions, organised in three groups of pseudo user journeys:
1. Actions of Onboarding: 
   - User Creation
   - First Login 
   - First Guide 
2. Actions of Work Alignment:
   - First Project 
   - First Story (_also used interchangeably as a task_) 
   - First Activity 
   - First Outcome
3. Actions of Work Execution:
   - First Task
   - First Meeting
   - First Decision
- The different distributions across actions between clients reflect different "ways of working" by our clients. For example, some clients rely more heavily on execution of tasks with fewer actions of alignment than others, while others use stories and tasks interchangeably. We can glean from this which clients follow more closely the agile framework (i.e. clear differentiation between stories and tasks), which ones use the product more heavily for productivity management (i.e. focus heavily on tasks), and which ones are more results oriented instead of process oriented (i.e. higher volume of outcomes specified).
- By adding user segmentation, the same analysis from the bullet above could be carried out by user roles, teams, and even individual collaborators.

### Action Lags

- Analyses how fast users engage for the first time with each of these actions.
- The horizontal axis is the numbers of days since a user was creted.
- The vertical axis tells us how many users took each of this actions at each day since their user was created.
- The ideal scenario looks as an L shaped curve (90°) overlapping exactly the x and y axis. This would mean all users executed for the first time the action on the same day their user was created on the system (i.e. on day zero).
- The ideal scenario tends to be approximated with most (relevant) clients for first log in, as most users log in the same day their users are created, or within the first couple of days, save for a few outliers.
- The pattern is similar for the first time users visit a guide, though this is primarily driven by users being automatically prompted by the system to a guide the first time they log in to the system.
- The first time users create projects also tends to have a fast uptake by some users, but a considerably smaller fraction of our total users (~ 3%).
- The first time users create a task tends to be relatively fast, though spreadout between the first three weeks since their user accounts where created, reflecting a more sloped adoption curve.
- Creation of first stories, activities, outcomes, meetings, and decisions have a more irregular adoption curve, whose explanations might be driven more by context not present in the data.

### Value Waste

- Under the (valid) assumption that by using our product our clients improve the management and execution of their projects, every day that passes since a user was given access to the product until they carry out any of these actions can be considered a wasteful day, and a day we were not directly delivering value to our customers.
- For this we came up with the KPI/ lead measure of "wasted days", which measures the time users delay in taking action on the different features provided by the product. It consists on the number of days from the day of user creation to the first time each action was done for the first time, and we measure the mean and median number of days, and the total days aggregated for each client.
- By us implementing interventions/nudges which minimimse these measures, we accelerate user oboarding, adoption, and engagement, as well as deliver value to clients at larger and faster scale.
- Directionally, by helping our clients minimise these measures on the set of work execution actions, we are helping them become MORE AGILE by increasing throughput.
- Additionally, by helping our clients minimise these measures on the set of work execution actions AND the set of work alignment actions, we are helping them become LEANER by increasing throughput and continuous improvement.

### Action Sequences

- Analyses sequence of events and actions carried out by our users when using our product. 
- Our objective is to understand the prior probabilities of sequence of events, in order to predict the next most likely event given a sequence, and suggest them to our users to drive engagement and better use of the product.
- For this we used Collocation Analysis, a technique from computational linguistics which estimates the likelihood that sequences of words following each other occur at probability different from chance. We explore collocation analysis as a potential candidate for producing a "next-best action" recommender system, by exploiting the assumption that a sequence of user actions have a linear logic, hence some actions groups of actions tend to co-occur together more or less often than chance.
- Actions tend to cluster around product features, hence a user working on an aspect of a task will tend to carry out a following action related to that same task or another task, while the same applies to stories, activities, etc.
- There appear to be many activities which are carried out in bulk, suchs as:
   - Changing the assignee id of a group of tasks.
   - Changing the due date of a group of stories.
- Some sequences of actions are also defined by the topology of the product, such as carrying out one activity forces the user to carry out another, for example:
   - Changing the due date of an outcome forces the action of recurrently publishing the outcome.
- Collocations analysis was effective when learned over all user actions across all clients. However, to glean insights on action sequences for a specific client, we would need to have a robust set of data for that client, which is not the case for most of the current instances.
- For more details see repo: https://github.com/fbe-st/collocation_analysis_of_user_actions

### User Transitions

- Tracks the activity status for every user in the system, following the below taxonomy:
- New User: Users which have less than 30 days since being created in the system.
- Active User: Users which still log in into the system and have more than 30 days since created in the system.
- Idel Users: Users who have not logged in into the system for more than 30 days.
- Resurrected Users: Users which were idle for more than 60 days and logged in once again into the system.
- An important measure to minimize here is the number of users which transition from New User straight to Idle User, implying that new users did not identify value in the product for them and didn't return to the platform asfter a trying it a few times. This is something which can be significantly fixed via interventions, nudges, and exposure to the Knowledge Base.
- Of special interest is seeing the amount of resurrected users in June 2020 as compared to May, particularly considering that it represents only the first 10 days of the month. It suggests that with relaxation of lockdown and an emphasis now of getting companies back to work, mamy users are returning to Sharktower.


## IMPORTANT
All data resources are not stored in this repo, and were pre-processed by separate scripts which belong to the respective repos of each individual analysis.
