module LordOfTheRings.Enemy.Cards.Grishnakh (grishnakh) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getCaptives, getToIsengard)

newtype Grishnakh = Grishnakh EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

grishnakh :: EnemyCard Grishnakh
grishnakh = enemy Grishnakh Cards.grishnakh

{- | Forced: After Grishnákh enters play, the first player attaches 1 of the Captives to
him. If Grishnákh is defeated, place the attached Captive faceup underneath To Isengard!
Forced: At the end of the quest phase, deal 1 damage to the attached Captive.
-}
instance HasAbilities Grishnakh where
  getAbilities (Grishnakh a) =
    [ mkAbility a 1 $ forced $ MatchingEnemyEntersPlayWindow After (EnemyWithId a.id)
    , mkAbility a 2 $ forced $ ThisEnemyDefeatedWindow When a.id
    , mkAbility a 3 $ forced $ PhaseEndsWindow When QuestPhase
    ]

-- | The Captive attached to him is the one he guards: placed under To Isengard! as he is
-- defeated, it is guarded by To Isengard! again, not freed as he leaves play.
instance RunMessage Grishnakh where
  runMessage msg e@(Grishnakh attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      captives <- getCaptives
      chooseOrRunOneM lead do
        questionLabeled "grishnakh.captive"
        causedBy attrs
        targets captives \h -> guardCharacterWith h attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      attached <- charactersGuardedBy attrs.cardId
      toIsengard <- getToIsengard
      for_ toIsengard \o -> for_ attached (`guardCharacterWith` o)
      pure e
    UseCardAbility _ (isSource attrs -> True) 3 _ -> do
      attached <- charactersGuardedBy attrs.cardId
      for_ attached \c -> dealDamage attrs c 1
      pure e
    _ -> Grishnakh <$> runMessage msg attrs
