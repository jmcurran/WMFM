# Stage 28 version numbering note

Stage 28 uses the version family `0.2.4.xxx`, where `xxx` is a three-digit build number.

Earlier Stage 28 runners allowed the final component to collapse from `001`, `002`, etc. to ordinary integer formatting such as `4`. Stage 28.4 corrects the completed 28.3 version from `0.2.4.4` to `0.2.4.005`.

For the remainder of this workstream, stage runners should preserve the width of the build field. For example:

- `0.2.4.005` should advance to `0.2.4.006`
- `0.2.4.099` should advance to `0.2.4.100`

The version should remain in four-component form during Stage 28, with the first three components fixed as `0.2.4` unless explicitly changed.
