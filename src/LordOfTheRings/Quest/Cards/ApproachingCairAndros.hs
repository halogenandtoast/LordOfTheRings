module LordOfTheRings.Quest.Cards.ApproachingCairAndros (approachingCairAndros) where

import LordOfTheRings.Keyword (Keyword (Siege))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ApproachingCairAndros = ApproachingCairAndros QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | If the players defeat this stage, they have won the game: it is the last in the quest
-- deck whichever stage the players bypassed.
approachingCairAndros :: QuestCard ApproachingCairAndros
approachingCairAndros = quest ApproachingCairAndros Cards.approachingCairAndros

-- | If any player's threat is 37 or higher, Approaching Cair Andros gains siege.
instance HasModifiersFor ApproachingCairAndros where
  getModifiersFor (ApproachingCairAndros a) = do
    high <- selectAny (PlayerWithThreatAtLeast 37)
    when high $ modified a a [GainKeyword Siege]

-- | Forced: At the end of each round, raise each player's threat by 2.
instance HasAbilities ApproachingCairAndros where
  getAbilities (ApproachingCairAndros a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage ApproachingCairAndros where
  runMessage msg q@(ApproachingCairAndros attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid 2
      pure q
    _ -> ApproachingCairAndros <$> runMessage msg attrs
