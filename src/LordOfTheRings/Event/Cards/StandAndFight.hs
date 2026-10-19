module LordOfTheRings.Event.Cards.StandAndFight (standAndFight) where

import LordOfTheRings.Card (Card (..), CardCost (..), CardDef (..), toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype StandAndFight = StandAndFight EventAttrs
  deriving anyclass (IsEvent)
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
      choices <- fmap concat $ for order \owner -> do
        discard <- (.discard) <$> getPlayer owner
        pure
          [ targetLabel (CardIdTarget pc.id) [SpendResources pid Spirit x, RemoveFromDiscard owner pc.id, PutAllyIntoPlayUnder pid pc]
          | PlayerCard pc <- discard
          , cdCardType (toCardDef pc) == AllyType
          , Just (StaticCost x) <- [cdCost (toCardDef pc)]
          , x <= spirit
          ]
      unless (null choices) $ push $ questionLabelWith (ikey "label.chooseAllyToPutIntoPlay") (Just $ toTarget attrs) [] pid choices
      pure e
    _ -> StandAndFight <$> runMessage msg attrs
   where
    resourcesIn pid = do
      heroes <- traverse getHero =<< select (HeroControlledBy (PlayerWithId pid) <> HeroWithSphere Spirit)
      pure $ sum (map (.resources) heroes)
