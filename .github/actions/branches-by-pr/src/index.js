const core = require("@actions/core");
const github = require("@actions/github");

async function run() {
    try {
        const userSuppliedPrId = core.getInput("pr-id");
        const token = core.getInput("repo-token");

        const prNumber = getPrNumber(userSuppliedPrId);

        if (!prNumber) {
            core.setFailed("Pull request number was neither set by user nor obtainable by context");
        }

        const octokit = new github.getOctokit(token);

        const response = await octokit.pulls.get({
            owner: github.context.repo.owner,
            repo: github.context.repo.repo,
            pull_number: prNumber
        });

        core.setOutput("base", response.data.base.ref);
        core.setOutput("head", response.data.head.ref);
    } catch (error) {
        core.error(error);
        core.setFailed(error.message);
    }
}

function getPrNumber(userSuppliedPrId) {
    if (isNaN(userSuppliedPrId)) {
        if (!github.context.payload.pull_request) {
            return undefined;
        }
        return github.context.payload.pull_request.number;
    } else {
        return userSuppliedPrId;
    }
}

run();