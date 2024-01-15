import numpy as np

lol = np.array([10, 13, 16, 21, 30, 45])
lol_rec = np.cumsum(
    np.insert(
        np.cumsum(
            np.insert(np.cumsum(np.insert(np.diff(np.diff(np.diff(lol))), 0, 0)), 0, 3)
        ),
        0,
        10,
    )
)

print(lol_rec)
