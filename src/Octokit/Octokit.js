import { Octokit, App } from "octokit"
import { throttling } from "@octokit/plugin-throttling"
import { request } from "@octokit/request"
const ThrottledOctokit = Octokit.plugin(throttling)

export function octokit(auth) {
    return () => new ThrottledOctokit({
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
}

export function requestImpl(client, route, params) {
    return () => client.request(route, params)
}
