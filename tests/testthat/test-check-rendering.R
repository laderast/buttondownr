httptest2::with_mock_dir("testing", {

test_that("rendering works", {
  response <- send_email("test_email.rmd")

  resp_code <- response |> httr2::resp_status()
  expect_equal(resp_code, 201)
})
}
)

httptest2::with_mock_dir("testing", {

  test_that("send image works", {
    response <- send_image("test_pic.png")

    resp_code <- response |> httr2::resp_status()
    expect_equal(resp_code, 201)
  })
}
)

