import org.scalatest._

case class Record(country: String, state: String, city: String, item: String)

class HierarchySpec extends FlatSpec with Matchers with BeforeAndAfter {

  trait Fixture {

    val defaultFunctions = List(
      (r: Record) => r.country,
      (r: Record) => r.state,
      (r: Record) => r.city,
      (r: Record) => r.item)


    val records = List(
      Record("USA", "Texas", "Denton", "123"),
      Record("USA", "Oklahoma", "Oklahoma City", "345"),
      Record("Canada", "British Columbia", "Vancouver", "766"),
      Record("Canada", "Ontario", "Toronto", "3643"),
      Record("USA", "Texas", "Dallas", "7346")
    )

    val hierBuilder = new HierarchyFromRecords[Record]()
    val result = hierBuilder.buildHierarchy(records, defaultFunctions)
  }

    "buildTree" should "have 2 items in the list" in new Fixture {
      result should have size 2
    }

    it should "have USA at the top level" in new Fixture {
      result.map(_.id) should contain("USA")
    }

    it should "have Canada at the top level" in new Fixture {
      result.map(_.id) should contain("Canada")
    }

    it should "have Texas beneath USA" in new Fixture {
      result.filter(_.id == "USA").head.children.map(_.id) should contain("Texas")
    }
}
