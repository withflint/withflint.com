module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import CognitiveComplexity
import Documentation.ReadmeLinksPointToCurrentVersion
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoRecursiveUpdate
import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUselessSubscriptions
import NoUnapprovedLicense
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ Simplify.rule Simplify.defaults
    , NoMissingSubscriptionsCall.rule
    , NoUselessSubscriptions.rule
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , Documentation.ReadmeLinksPointToCurrentVersion.rule
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    , NoUnapprovedLicense.rule
        { allowed = [ "BSD-3-Clause", "MIT", "Apache-2.0", "MPL-2.0" ]
        , forbidden = [ "GPL-3.0-only", "GPL-3.0-or-later" ]
        }
    , CognitiveComplexity.rule 20
    ]
