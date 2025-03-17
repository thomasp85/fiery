# Assertions throw errors

    Code
      rs(stack$add("string", "id"))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `handler` must be a function, not the string "string".

---

    Code
      rs(stack$add(c(min, max), "id"))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `handler` must be a function, not a list.

---

    Code
      rs(stack$add(min, 3))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `id` must be a single string, not the number 3.

---

    Code
      rs(stack$add(min, 3:5))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `id` must be a single string, not an integer vector.

---

    Code
      rs(stack$add(min, "min", "position"))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `pos` must be a whole number, not the string "position".

---

    Code
      rs(stack$add(min, "min", 1:4))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `pos` must be a whole number, not an integer vector.

---

    Code
      rs(stack$add(min, "min", 1.5))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `pos` must be a whole number, not the number 1.5.

---

    Code
      rs(stack$remove(1))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `id` must be a single string, not the number 1.

---

    Code
      rs(stack$remove(c("1", "2")))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `id` must be a single string, not a character vector.

---

    Code
      rs(stack$position(1))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `id` must be a character vector, not the number 1.

---

    Code
      rs(stack$contains(1))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `id` must be a character vector, not the number 1.

