# Stage 29.3 version recovery note

Stage 29.2 completed with DESCRIPTION at 0.2.5.027 after the stage-runner version logic followed the current file value instead of the intended Stage 29 build count.

Stage 29.3 resets the next attempted build to 0.2.5.004. The runner stores its per-stage attempt counter in the local git metadata so the first 29.3 run writes 0.2.5.004 exactly, and later failed reruns advance to 0.2.5.005, 0.2.5.006, and so on.

This note is workflow metadata only. It does not change app behaviour.
