|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | x  | x  | x  |
| r2 | x  | x  | x  | x  | x  |
| r3 | x  | x  | x  | x  | x  |

R2[2:5] = R2 - R1 * (R2lead / R1lead)
calculations: (1 division) + (c - 1) * (1 multiplication) = c

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | x  | x  | x  |
| r2 | 0  | x  | x  | x  | x  |
| r3 | x  | x  | x  | x  | x  |

R3[2:5] = R3 - R1 * (R3lead / R1lead)
calculations: (1 division) + (c - 1) * (1 multiplication) = c

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | x  | x  | x  |
| r2 | 0  | x  | x  | x  | x  |
| r3 | 0  | x  | x  | x  | x  |

R3[3:5] = R3 - R2 * (R3lead / R2lead)
calculations: (1 division) + (c - 2) * (1 multiplication) = c - 1

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | x  | x  | x  |
| r2 | 0  | x  | x  | x  | x  |
| r3 | 0  | 0  | x  | x  | x  |

R3[4:5] = R3 / R3lead
calculations: (1 division) * (c - 3) = c - 3

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | x  | x  | x  |
| r2 | 0  | x  | x  | x  | x  |
| r3 | 0  | 0  | 1  | x  | x  |

R2[4:5] = R2 - R3 * R2right
calculations: (1 multiplication) * (c - 3) = c - 3

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | x  | x  | x  |
| r2 | 0  | x  | 0  | x  | x  |
| r3 | 0  | 0  | 1  | x  | x  |

R2[4:5] = R2 / R2lead
calculations: (1 division) * (c - 3) = c - 3

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | x  | x  | x  |
| r2 | 0  | 1  | 0  | x  | x  |
| r3 | 0  | 0  | 1  | x  | x  |

R1[4:5] = R1 - R3 * R1right
calculations: (1 multiplication) * (c - 3) = c - 3

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | x  | 0  | x  | x  |
| r2 | 0  | 1  | 0  | x  | x  |
| r3 | 0  | 0  | 1  | x  | x  |

R1[4:5] = R1 - R2 * R1middle
calculations: (1 multiplication) * (c - 3) = c - 3

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | x  | 0  | 0  | x  | x  |
| r2 | 0  | 1  | 0  | x  | x  |
| r3 | 0  | 0  | 1  | x  | x  |

R1[4:5] = R1 / R1lead
calculations: (1 division) * (c - 3) = c - 3

|    | c1 | c2 | c3 | c4 | c5 |
|----|----|----|----|----|----|
| r1 | 1  | 0  | 0  | x  | x  |
| r2 | 0  | 1  | 0  | x  | x  |
| r3 | 0  | 0  | 1  | x  | x  |


c
c
c - 1
c - 3
c - 3
c - 3
c - 3
c - 3
c - 3

9c - 19