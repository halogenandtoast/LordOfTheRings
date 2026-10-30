module LordOfTheRings.Event.Cards.KeenAsLances (keenAsLances) where

import LordOfTheRings.Card (cdVictoryPoints, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gameScenario)

newtype KeenAsLances = KeenAsLances EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Add Keen as Lances to the victory display. Then, choose one: add 2 resources
-- to a hero's resource pool, draw 3 cards, or reduce your threat by 4.
keenAsLances :: EventCard KeenAsLances
keenAsLances = event KeenAsLances Cards.keenAsLances

-- | Reduce the cost to play Keen as Lances by 1 for each card worth no victory points in
-- the victory display.
instance HasModifiersFor KeenAsLances where
  getModifiersFor (KeenAsLances a) = do
    n <- getsGame (length . filter (maybe True (== 0) . cdVictoryPoints . toCardDef) . (.victoryDisplay) . toAttrs . gameScenario)
    when (n > 0) $ modifySelf a [CostModifier (negate n)]

instance RunMessage KeenAsLances where
  runMessage msg e@(KeenAsLances attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      addToVictoryDisplay card
      heroes <- select AnyHero
      chooseOneM pid do
        questionLabeled "keenAsLances.choose"
        causedBy attrs
        labeled "keenAsLances.resources" $ chooseOrRunOneM pid do
          questionLabeled "chooseHero"
          causedBy attrs
          targets heroes \h -> placeResources attrs h 2
        labeled "keenAsLances.draw" $ drawCards attrs pid 3
        labeled "keenAsLances.threat" $ lowerThreatBy attrs pid 4
      pure e
    _ -> KeenAsLances <$> runMessage msg attrs
