# How to maintain this package

This document has been created along with the project through the `elm-review new-package` command.

A lot of things should happen automatically for you. Here is an overview of what you should know and what to expect.

We'll discuss

- [After creation](#after-creation)
- [Writing rules](#writing-rules)
- [Example and preview configurations](#example-and-preview-configurations)
- [Publishing](#publishing)

This document and the set up created for you is aimed at helping you work and improving the quality of the package. You can however opt out of all of this if you encounter problems or it doesn't suit you for any reason. If that happens, please contact the `elm-review` maintainers so that they can work on improvements.

## After creation

Right after you have created the package, you should

### 4. (Can be done later) Create the project on GitHub

You can do this step at a later time if you prefer.
When you do, consider to

- Adding the `elm-review` tag, so that your project appears in [this list](https://github.com/topics/elm-review).
- [Adding a code of conduct](https://docs.github.com/en/github/building-a-strong-community/adding-a-code-of-conduct-to-your-project)
- [Adding issue and pull request templates](https://docs.github.com/en/github/building-a-strong-community/using-templates-to-encourage-useful-issues-and-pull-requests)
- [Setting guidelines for repository contributors](https://docs.github.com/en/github/building-a-strong-community/setting-guidelines-for-repository-contributors)

## Writing rules

You can read how to use `elm-review`'s API to write a rule [here](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule).

I highly recommend writing rules in a test-driven manner (TDD), because the tests you write will dramatically increase the quality of the rule, and because they will make the development quite easy. You can read more about it [here](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Test).

If you want to preview the documentation of the package, you can run `npm run preview-docs`, which will run a local version of [`elm-doc-preview`](https://elm-doc-preview.netlify.app/). If you see `docs.json` in your project, please commit it. This will allow you to share the latest version of the docs with collaborators using the online version of [`elm-doc-preview`](https://elm-doc-preview.netlify.app/).

### Adding a new rule

It is better if rule packages contain multiple rules dealing with the same or similar concerns, rather than a single rule. Therefore, you will likely add a new rule to the package at one point.

To do so, I recommend using the `elm-review new-rule` command. This will prompt you for the name of the rule, and then create a source file and a test file, and update the `elm.json` and the `README.md`, and insert the rule into the preview configurations.

## Example and preview configurations

In the rules generated by `new-package` and `new-rule`, and also in the README, you will see a section named "Try it out" which recommends using a command that looks like this:

```bash
elm-review --template <author>/<package name>/example --rules <rule name>
```

This enables people to run the rule without having to set up `elm-review`, add your package as a dependency, add the rules to their configuration and then run it. This is an easy way to try out the rules and see if they can be useful to them. In order for this to work, you will need to do a little bit of work, but this will be useful to you too!

There are two folders that will exist in your folder that will help make this work, `preview/` and `example/`. Both are review configurations and they serve similar purposes.

- `example/` is the configuration that works with the last **released** version (as a dependency). It should not change in a way that would not work with the latest released version (no unpublished rules, no new arguments, etc.). Examples in the documentation will use this configuration, which is why it should remain stable.
- `preview/` is the configuration that works with the current version of the source code (as a source directory). It can change however/whenever you see fit. You can use this configuration to let users/testers try out new rules or bug fixes to published rules **before releasing a new version**.

When the project gets created, you will only have a `preview/` folder. You will create `example/` (automatically) when you are ready to [publish the initial release](#initial-release), using `node maintenance/update-examples-from-preview.js` which will copy the contents of the `preview/` configuration and adapt it to use the package as a dependency instead of source directories. Before every release, you will have to run this same command, otherwise tests in the CI will fail when attempting to publish the package. You should ideally not change `example` yourself, and instead consider `preview/` as the source of truth.

In practice, you are not limited to a single example and a single preview configuration. You can add new configurations and name them however you want. The pre-made scripts will look for any project (containing an `elm.json` at its root) in directories and sub-directories whose name start with `preview`.
You can therefore have `preview/with-configuration-A/` and `preview/with-configuration-B/`, or `preview-with-configuration-A/` and `preview-with-configuration-B/`. When creating the example configurations from these, their names will be the same, except that "preview" in their names will be changed to "example".

## Publishing

### Initial release

The initial release has to be done manually. I recommend running the following script:

```bash
# Make sure your tests pass. Fix them if necessary
npm test

# Generate the example configurations
node maintenance/update-examples-from-preview.js
git add --all
git commit --message '1.0.0'

# Commit
git tag 1.0.0
git push --tags origin $(git_main_branch)
elm publish
```

### Successive releases

Contrary to the initial release, the CI will automatically try to publish a new version of the package when the version in the `elm.json` is bumped. There is **no need** to add the Git tag or to run `elm publish` yourself! More details [here](https://github.com/dillonkearns/elm-publish-action).

Here is a script that you can run to publish your package, which will help you avoid errors showing up at the CI stage.

```bash
npm run elm-bump

# Commit it all
git add --all
git commit # You'll need to specify a message
git push origin HEAD

# Now wait for CI to finish and check that it succeeded
```
