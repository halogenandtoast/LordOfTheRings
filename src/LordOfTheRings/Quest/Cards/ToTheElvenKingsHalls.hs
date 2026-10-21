module LordOfTheRings.Quest.Cards.ToTheElvenKingsHalls (toTheElvenKingsHalls) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype ToTheElvenKingsHalls = ToTheElvenKingsHalls QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

toTheElvenKingsHalls :: QuestCard ToTheElvenKingsHalls
toTheElvenKingsHalls = quest ToTheElvenKingsHalls Cards.toTheElvenKingsHalls

-- | The player guarding Gollum cannot play cards from his hand.
instance HasModifiersFor ToTheElvenKingsHalls where
  getModifiersFor (ToTheElvenKingsHalls a) = traverse_ (\g -> modified a (PlayerTarget g) [CannotPlay AnyCard]) =<< getGuard
