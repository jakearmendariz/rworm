load harness

@test "custom-1" {
  check '10 - 7 ' '3'
}

@test "custom-2" {
  check '-45 - 100' '-145'
}

@test "custom-3" {
  check '2 * 8 * 0 - 45 + 50' '5'
}

@test "custom-4" {
  check '3 * 6 - 10' '8'
}

@test "custom-5" {
  check '2 * 2 * 2 * 2 - 10' '6'
}