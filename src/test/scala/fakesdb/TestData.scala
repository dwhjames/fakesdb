package fakesdb

object TestData {
  val domainName = "mydomain"
  val items =
    Map(
      "0385333498" ->
        Set(
          "Title"   -> "The Sirens of Titan",
          "Author"  -> "Kurt Vonnegut",
          "Year"    -> "1959",
          "Pages"   -> "00336",
          "Keyword" -> "Book",
          "Keyword" -> "Paperback",
          "Rating"  -> "*****",
          "Rating"  -> "5 stars",
          "Rating"  -> "Excellent"
        ),
      "0802131786" ->
        Set(
          "Title"   -> "Tropic of Cancer",
          "Author"  -> "Henry Miller",
          "Year"    -> "1934",
          "Pages"   -> "00318",
          "Keyword" -> "Book",
          "Rating"  -> "****"
        ),
      "1579124585" ->
        Set (
          "Title"   -> "The Right Stuff",
          "Author"  -> "Tom Wolfe",
          "Year"    -> "1979",
          "Pages"   -> "00304",
          "Keyword" -> "Book",
          "Keyword" -> "Hardcover",
          "Keyword" -> "American",
          "Rating"  -> "****",
          "Rating"  -> "4 stars"
        ),
      "B000T9886K" ->
        Set(
          "Title"   -> "In Between",
          "Author"  -> "Paul Van Dyk",
          "Year"    -> "2007",
          "Keyword" -> "CD",
          "Keyword" -> "Trance",
          "Rating"  -> "4 stars"
        ),
      "B00005JPLW" ->
        Set(
          "Title"   -> "300",
          "Author"  -> "Zack Snyder",
          "Year"    -> "2007",
          "Keyword" -> "DVD",
          "Keyword" -> "Action",
          "Keyword" -> "Frank Miller",
          "Rating"  -> "***",
          "Rating"  -> "3 stars",
          "Rating"  -> "Not bad"
        ),
      "B000SF3NGK" ->
        Set(
          "Title"   -> "Heaven's Gonna Burn Your Eyes",
          "Author"  -> "Thievery Corporation",
          "Year"    -> "2002",
          "Rating"  -> "*****"
        )
    )
}
