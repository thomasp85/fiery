# Assertions throw errors

    Code
      stack$add("string", "id")
    Condition
      Error in `stack$add()`:
      ! `handler` must be a function, not the string "string".

---

    Code
      stack$add(c(min, max), "id")
    Condition
      Error in `stack$add()`:
      ! `handler` must be a function, not a list.

---

    Code
      stack$add(min, 3)
    Condition
      Error in `stack$add()`:
      ! `id` must be a single string, not the number 3.

---

    Code
      stack$add(min, 3:5)
    Condition
      Error in `stack$add()`:
      ! `id` must be a single string, not an integer vector.

---

    Code
      stack$add(min, "min", "position")
    Condition
      Error in `stack$add()`:
      ! `pos` must be a whole number, not the string "position".

---

    Code
      stack$add(min, "min", 1:4)
    Condition
      Error in `stack$add()`:
      ! `pos` must be a whole number, not an integer vector.

---

    Code
      stack(add(min, "min", 1.5))
    Condition
      Error in `add()`:
      ! could not find function "add"

---

    Code
      stack$remove(1)
    Condition
      Error in `stack$remove()`:
      ! `id` must be a single string, not the number 1.

---

    Code
      stack$remove(c("1", "2"))
    Condition
      Error in `stack$remove()`:
      ! `id` must be a single string, not a character vector.

---

    Code
      stack$position(1)
    Condition
      Error in `stack$position()`:
      ! `id` must be a character vector, not the number 1.

---

    Code
      stack$contains(1)
    Condition
      Error in `stack$contains()`:
      ! `id` must be a character vector, not the number 1.

