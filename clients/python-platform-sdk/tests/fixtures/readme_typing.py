from datetime import date

from farm_sdk import Buyer, Field, Lot, Sale

lot = Lot.get("North Meadow, 1st cut 2025")
in_barn: int = lot.in_barn
sales: list[Sale] = lot.sales
wheat = next(h for h in Field.get("hilltop").harvests if h.status == "planted")
wheat.record_harvest(harvested_on=date(2027, 7, 28), tonnes=61.5)
wheat.sell_harvest(buyer=Buyer.get("stone-mill"), price_per_tonne=212)
running_low = Lot.where(Lot.in_barn < 50)
emptied_by: list[Sale] = running_low.around(Lot.sales).all()

lot.sell_hay(bales=20, buyer="Neighbour")
lot.sell_hay(sale_nr="S-1", buyer="Neighbour", bales="20")
Lot.where(Lot.in_barn < "about forty")
lot.field.margin
status: str = wheat.status
