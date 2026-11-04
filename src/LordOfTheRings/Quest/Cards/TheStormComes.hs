module LordOfTheRings.Quest.Cards.TheStormComes (theStormComes) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))

newtype TheStormComes = TheStormComes QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 copy of The Storm Comes in the victory display, as its card
-- def says.
theStormComes :: QuestCard TheStormComes
theStormComes = quest TheStormComes Cards.theStormComes

-- | While this quest is in the victory display, the first ally played by each player each
-- round does not require a resource match: until he has played one this round, his heroes
-- have every sphere's resource icon for allies.
instance HasModifiersFor TheStormComes where
  getModifiersFor (TheStormComes a) = whenM (inVictoryDisplay a.cardId) do
    -- read from the effects, as the modifiers are being gathered
    effects <- getsGame gameEffects
    let played pid = any (\e -> effectTarget e == PlayerTarget pid && PlayedCardTypeThisRound AllyType `elem` effectModifiers e) effects
    players <- select AnyPlayer
    for_ (filter (not . played) players) \pid -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> modified a h [GainResourceIconFor (CardWithType AllyType) s | s <- [Leadership, Lore, Spirit, Tactics]]
