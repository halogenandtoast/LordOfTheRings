module LordOfTheRings.Event.Cards.StandAndFight (standAndFight) where

import LordOfTheRings.Card (Card (..), CardCost (..), CardDef (..), toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype StandAndFight = StandAndFight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose an ally with a printed cost of X in any player's discard pile. Put that
-- ally into play under your control. (The chosen ally can belong to any sphere.)
--
-- X is paid in Spirit resources once the ally is chosen, so only allies the player can
-- pay for are offered.
standAndFight :: EventCard StandAndFight
standAndFight = event StandAndFight Cards.standAndFight

instance RunMessage StandAndFight where
  runMessage msg e@(StandAndFight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      spirit <- resourcesIn pid
      order <- getPlayerOrder
      allies <- fmap concat $ for order \owner -> do
        discarded <- (.discard) <$> getPlayer owner
        pure
          [ (owner, pc, x)
          | PlayerCard pc <- discarded
          , cdCardType (toCardDef pc) == AllyType
          , Just (StaticCost x) <- [cdCost (toCardDef pc)]
          , x <= spirit
          ]
      chooseOneM pid do
        questionLabeled "chooseAllyToPutIntoPlay"
        causedBy attrs
        for_ allies \(owner, pc, x) -> targeting pc do
          spendResources pid Spirit x
          removeFromDiscard owner pc.id
          push $ PutAllyIntoPlayUnder pid pc
      pure e
    _ -> StandAndFight <$> runMessage msg attrs
   where
    resourcesIn pid = do
      heroes <- traverse getHero =<< select (HeroControlledBy (PlayerWithId pid) <> HeroWithSphere Spirit)
      pure $ sum (map (.resources) heroes)
