const { Octokit, App } = require("octokit")
const { throttling } = require("@octokit/plugin-throttling")
const { request } = require("@octokit/request")
const ThrottledOctokit = Octokit.plugin(throttling)

exports.octokit = (auth) => () => new ThrottledOctokit({
    auth,
    throttle: {
        onRateLimit: (retryAfter, options, octokit) => {
            octokit.log.warn(
                `Request quota exhausted for request ${options.method} ${options.url}`
            )

            if (options.request.retryCount === 0) {
                // only retries once
                octokit.log.info(`Retrying after ${retryAfter} seconds!`)
                return true
            }
        },
        onAbuseLimit: (retryAfter, options, octokit) => {
            // does not retry, only logs a warning
            octokit.log.warn(
                `Abuse detected for request ${options.method} ${options.url}`
            )
        },
    }
})

exports.requestImpl = (client, route, params) => () => client.request(route, params)
