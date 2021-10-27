# setup_books <- function() {
#   latest_file <- list_files() %>%
#     filter(
#       type == "gnca",
#       last_date == max(last_date)
#     )
#   xml_root_node <- read_cash_xml(latest_file)
#   books <- get_books(xml_root_node)
#   return(books)
# }


GNU_XML_PREFIXES <- '<gnc-v2 xmlns:gnc="http://www.gnucash.org/XML/gnc" xmlns:act="http://www.gnucash.org/XML/act"
  xmlns:book="http://www.gnucash.org/XML/book" xmlns:cd="http://www.gnucash.org/XML/cd"
  xmlns:cmdty="http://www.gnucash.org/XML/cmdty" xmlns:price="http://www.gnucash.org/XML/price"
  xmlns:slot="http://www.gnucash.org/XML/slot" xmlns:split="http://www.gnucash.org/XML/split"
  xmlns:trn="http://www.gnucash.org/XML/trn" xmlns:ts="http://www.gnucash.org/XML/ts"
  xmlns:sx="http://www.gnucash.org/XML/sx" xmlns:bgt="http://www.gnucash.org/XML/bgt"
  xmlns:recurrence="http://www.gnucash.org/XML/recurrence">'

test_that("Read xml files", {
  latest_file <- structure(list(
    path = structure(c(`/home/victor/Escritorio/Cash/inst/extdata/20210721_003525_gnucash_export_Yo.gnca` =
                         "/home/victor/Escritorio/Cash/inst/extdata/20210721_003525_gnucash_export_Yo.gnca"),
                       class = c(
                        "fs_path",
                        "character"
                      )),
    name = "20210721_003525_gnucash_export_Yo.gnca",
    last_date = structure(18829, class = "Date"), other = "003525",
    account = structure(1L, .Label = "Yo", class = "factor"),
    type = "gnca"
  ), row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ))

  expect_equal(class(read_cash_xml(latest_file)), c("xml_document", "xml_node"))
})

test_that("Get books from xml", {
  xml_root_node <- xml2::read_xml("<gnc-v2 xmlns:gnc='http://www.gnucash.org/XML/gnc' xmlns:book='http://www.gnucash.org/XML/book'><gnc:book><book:id>'a'</book:id><book:account>'b'</book:account></gnc:book></gnc-v2>")
  books <- get_books(xml_root_node)

  expect_equal(class(books), c("xml_nodeset"))
  expect_gte(length(books), 1)
  expect_equal(xml2::as_list(books) %>% dput(), list(list(id = list("'a'"), account = list("'b'"))))
})

test_that("Get book metadata", {
  xml_root_node <- xml2::read_xml(
    glue::glue('
      {GNU_XML_PREFIXES}
        <gnc:book>
          <book:id type="guid">98f703dce5304f7eb1fc817826380160</book:id>
          <gnc:count-data cd:type="commodity">1</gnc:count-data>
          <gnc:count-data cd:type="account">72</gnc:count-data>
          <gnc:count-data cd:type="transaction">267</gnc:count-data>
          <gnc:commodity version="2.0.0">
            <cmdty:space>ISO4217</cmdty:space>
            <cmdty:id>EUR</cmdty:id>
          </gnc:commodity>
           <gnc:transaction version="2.0.0">
            <trn:date-posted>
              <ts:date>2021-05-01 00:34:59 +0200</ts:date>
            </trn:date-posted>
            <trn:date-entered>
              <ts:date>2021-05-01 00:35:36 +0200</ts:date>
            </trn:date-entered>
          </gnc:transaction>
          <gnc:transaction version="2.0.0">
            <trn:date-posted>
              <ts:date>2021-05-01 00:38:26 +0200</ts:date>
            </trn:date-posted>
            <trn:date-entered>
              <ts:date>2021-05-01 00:38:56 +0200</ts:date>
            </trn:date-entered>
          </gnc:transaction>
        </gnc:book>
    </gnc-v2>')
  )
  book <- get_books(xml_root_node)[[1]]

  metadata <- get_book_metadata(book)
  expect_equal(class(metadata), c("tbl_df", "tbl", "data.frame"))
  expect_equal(
    dput(metadata),
    structure(list(
      Dato = c(
        "Fecha ini", "Fecha fin", "Dinero", "Cuentas",
        "Transacciones"
      ),
      Valor = c(
        "2021-04-30T22:34:59", "2021-04-30T22:38:56",
        "1", "72", "267"
      ),
      Descripcion = c(
        "Primera fecha disponible",
        "Última fecha disponible", "Numero de monedas usadas", "Numero de cuentas creadas",
        "Numero de movimientos"
      )
    ),
    row.names = c(NA, -5L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )
    )
  )
})

test_that("Get currencies", {
  xml_root_node <- xml2::read_xml(
    glue::glue('
      {GNU_XML_PREFIXES}
        <gnc:book>
          <gnc:commodity version="2.0.0">
            <cmdty:space>ISO4217</cmdty:space>
            <cmdty:id>EUR</cmdty:id>
          </gnc:commodity>
        </gnc:book>
    </gnc-v2>')
  )
  book <- get_books(xml_root_node)[[1]]

  currencies <- get_book_currencies(book)
  expect_equal(class(currencies), c("tbl_df", "tbl", "data.frame"))
  expect_gte(length(currencies), 1)
  expect_equal(
    dput(currencies),
    structure(list(Id = 1L, ISO = "ISO4217", Divisa = "EUR"),
      row.names = c(NA, -1L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
})


test_that("Get parent list", {
  skip("Not really developed, TODO: Separate get_accoutns from get_parent_list")
  xml_root_node <- xml2::read_xml(
    glue::glue('
      {GNU_XML_PREFIXES}
        <gnc:book>
          <gnc:account version="2.0.0">
            <act:name>Ajustes Bizum</act:name>
            <act:id type="guid">ad653905d4b1d3ce42a0f3c25fb1fad6</act:id>
            <act:type>EXPENSE</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Ajustes</act:description>
            <act:parent type="guid">6df1ed637347daee156997bebd11924a</act:parent>
          </gnc:account>
        </gnc:book>
    </gnc-v2>')
  )
  book <- get_books(xml_root_node)[[1]]

  data <- get_book_accounts(book)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_gte(length(data), 1)
  expect_equal(data$cuenta_base, "Ajustes Bizum")
})

test_that("Get accounts", {
  xml_root_node <- xml2::read_xml(
    glue::glue('
      {GNU_XML_PREFIXES}
        <gnc:book>
          <gnc:account version="2.0.0">
            <act:name>Ajustes Bizum</act:name>
            <act:id type="guid">ad653905d4b1d3ce42a0f3c25fb1fad6</act:id>
            <act:type>EXPENSE</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Ajustes</act:description>
            <act:parent type="guid">6df1ed637347daee156997bebd11924a</act:parent>
          </gnc:account>
        </gnc:book>
    </gnc-v2>')
  )
  book <- get_books(xml_root_node)[[1]]

  data <- get_book_accounts(book)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_gte(length(data), 1)
})

test_that("Get multiple accounts", {
  xml_root_node <- xml2::read_xml(
    glue::glue('
      {GNU_XML_PREFIXES}
        <gnc:book>
          <gnc:account version="2.0.0">
            <act:name>Ajustes Bizum</act:name>
            <act:id type="guid">ad653905d4b1d3ce42a0f3c25fb1fad6</act:id>
            <act:type>EXPENSE</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Ajustes</act:description>
            <act:parent type="guid">6df1ed637347daee156997bebd11924a</act:parent>
          </gnc:account>
          <gnc:account version="2.0.0">
            <act:name>ROOT</act:name>
            <act:id type="guid">6df1ed637347daee156997bebd11924a</act:id>
            <act:type>ROOT</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Ajustes</act:description>
            <act:parent type="guid">b</act:parent>
          </gnc:account>
        </gnc:book>
    </gnc-v2>')
  )

  book <- get_books(xml_root_node)[[1]]

  data <- get_book_accounts(book)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(data), 2)
})

test_that("Get parents", {
  xml_root_node <- xml2::read_xml(
    glue::glue('
      {GNU_XML_PREFIXES}
        <gnc:book>
          <gnc:account version="2.0.0">
            <act:name>Ajustes Bizum</act:name>
            <act:id type="guid">ad653905d4b1d3ce42a0f3c25fb1fad6</act:id>
            <act:type>EXPENSE</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Ajustes</act:description>
            <act:parent type="guid">6df1ed637347daee156997bebd11924a</act:parent>
          </gnc:account>
          <gnc:account version="2.0.0">
            <act:name>ROOT</act:name>
            <act:id type="guid">6df1ed637347daee156997bebd11924a</act:id>
            <act:type>ROOT</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Ajustes</act:description>
            <act:parent type="guid">b</act:parent>
          </gnc:account>
        </gnc:book>
    </gnc-v2>')
  )
  book <- get_books(xml_root_node)[[1]]
  accounts <- get_book_accounts(book) %>%
    get_parents_list()

  data <- find_parents("ad653905d4b1d3ce42a0f3c25fb1fad6", accounts)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(data), 2)
  expect_equal(dput(data$lista_cuentas), list(c("Ajustes Bizum", "ROOT"), "ROOT"))
})

# TODO: Test get_parents_list



test_that("Get transactions", {
  xml_root_node <- xml2::read_xml(
    glue::glue('
      {GNU_XML_PREFIXES}
        <gnc:book>
          <gnc:account version="2.0.0">
            <act:name>Ajustes Bizum</act:name>
            <act:id type="guid">ad653905d4b1d3ce42a0f3c25fb1fad6</act:id>
            <act:type>EXPENSE</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Ajustes</act:description>
            <act:parent type="guid">6df1ed637347daee156997bebd11924a</act:parent>
          </gnc:account>

         <gnc:transaction version="2.0.0">
            <trn:id type="guid">5c81ece6160040aea4678b0fe261c7e3</trn:id>
            <trn:currency>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>EUR</cmdty:id>
            </trn:currency>
            <trn:date-posted>
              <ts:date>2021-03-08 22:05:04 +0100</ts:date>
            </trn:date-posted>
            <trn:date-entered>
              <ts:date>2021-03-24 22:05:49 +0100</ts:date>
            </trn:date-entered>
            <trn:description>Cena Pijoteria casa</trn:description>
            <trn:slots>
              <slot>
                <slot:key>notes</slot:key>
                <slot:value type="string">Bizum berru</slot:value>
              </slot>
            </trn:slots>
            <trn:splits>
              <trn:split>
                <split:id type="guid">4fb34bf34c524a5ba2404c4a40c241a9</split:id>
                <split:reconciled-state>n</split:reconciled-state>
                <split:value>-1350/100</split:value>
                <split:quantity>-1350/100</split:quantity>
                <split:account type="guid">ad653905d4b1d3ce42a0f3c25fb1fad6</split:account>
              </trn:split>
              <trn:split>
                <split:id type="guid">e5682c868f9f4e38acb6c712a7c1da0f</split:id>
                <split:reconciled-state>n</split:reconciled-state>
                <split:value>1350/100</split:value>
                <split:quantity>1350/100</split:quantity>
                <split:account type="guid">ad653905d4b1d3ce42a0f3c25fb1fad6</split:account>
              </trn:split>
            </trn:splits>
          </gnc:transaction>
        </gnc:book>
    </gnc-v2>')
  )

  book <- get_books(xml_root_node)[[1]]
  accounts <- get_book_accounts(book)

  data <- get_book_transactions(book, accounts)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(data), 2)
})
