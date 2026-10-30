module LordOfTheRings.Quest.Cards.TheShadowWorld (theShadowWorld) where

import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

newtype TheShadowWorld = TheShadowWorld QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
theShadowWorld :: QuestCard TheShadowWorld
theShadowWorld = quest TheShadowWorld Cards.theShadowWorld

-- | Forced: At the end of the refresh phase, each player discards the top X cards of his
-- deck, where X is the number of Undead enemies engaged with him.
instance HasAbilities TheShadowWorld where
  getAbilities (TheShadowWorld a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]

instance RunMessage TheShadowWorld where
  runMessage msg q@(TheShadowWorld attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> do
        x <- length <$> select (EnemyWithTrait Undead <> EnemyEngagedWith (PlayerWithId pid))
        when (x > 0) $ discardTopOfDeck pid x
      pure q
    _ -> TheShadowWorld <$> runMessage msg attrs
