#### [How I reverted several git commits in a single commit](http://archlinux.me/dusty/2011/02/26/how-i-reverted-several-git-commits-in-a-single-commit/)

I hate to publicly admit this, but I recently made four commits that should have been merged into one commit, including two with embarrassing commit messages like, “third commit without testing, for shame!” I'm thoroughly shocked that fellow coder, Dan McGee hasn't already attacked me for my misdemeanor.

Please forgive me, I was tired and in a hurry and was working on something that was easier tested on the production server and most certainly deserve to be attacked by a velociraptor.

To complicate matters, there was a fifth commit in the middle of these four commits that was pertaining to an irrelevant task, and several other users had committed changes after those commits.

Fastforward to today. Those four commits made in a hurry, now have to be reverted. As with any task, there are several ways to do this using git, but none of them are immediately obvious. git reset –keep was out of the question because of the newer commits. I think I could have git rebased the changes out of a new branch and merged them, but the method that made the most sense to me was to revert them independently, and then squash them.

Here's how my history looked:

    A–C1–C2–Ex–C3–C4–O1–O2–O3

The four C commits are the ones I want to revert. Ex was an extraneous commit I want to keep and the O commits were made by other authors later.

This was the desired end state:

    A–C1–C2–Ex–C3–C4–O1–O2–O3–R

where R is a commit reverting the changes made in the four C commits. I didn't want to simply erase the C commits, (which can be done easily with git rebase), as embarrassing as they are, because they are public history that had been pushed to other users.

My process was to run git revert several times:

    git revert C4
    git revert C3
    git revert C2
    git revert C1

Possibly there is a way to do all of this in one command, I'm not sure. This left me with:

    A–C1–C2–Ex–C3–C4–O1–O2–O3–R1–R2–R3–R4

where the four R commits are reversions of the four C commits.

Then I ran:

    git rebase -i HEAD~5

git rebase -i is my favourite method of rebasing. It lists the five most recent commits in vim asking me what to do with each one. You can choose several options for each commit. Here is what I chose:

    pick O3
    reword R1
    squash R2
    squash R3
    squash R4

pick O3 says to include that commit and leave it unchanged. When rebasing, I usually go one commit earlier than I expect to make sure I'm modifying the correct history. The reword commit simply allows me to change the commit message of R1 to “Revert the XYZ changes because I no longer need them” The squash commits mean that those three R commits are merged into the previous commit — R1. And my end state is as desired:

    A–C1–C2–Ex–C3–C4–O1–O2–O3–R

I'm pretty sure there are other other ways to do this. I chose this multi-step process because it allows me to understand what is going on at each step and to double check that I haven't accidentally removed, merged, or reverted a commit I didn't mean to.
