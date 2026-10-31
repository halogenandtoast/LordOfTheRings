module LordOfTheRings.Enemy.Cards.OrcTrickster (orcTrickster) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)

newtype OrcTrickster = OrcTrickster EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcTrickster :: EnemyCard OrcTrickster
orcTrickster = enemy OrcTrickster Cards.orcTrickster

-- | Forced: After Orc Trickster is turned faceup as a hidden card, it makes an immediate
-- attack against the engaged player.
-- Forced: When Orc Trickster would be damaged, a random player takes it facedown as a
-- hidden card in his play area instead.
instance HasAbilities OrcTrickster where
  getAbilities (OrcTrickster a) =
    [ mkAbility a 1 $ forced $ HiddenCardTurnedFaceupWindow After (toTarget a)
    , mkAbility a 2 $ forced $ DealtDamageWindow When (toTarget a)
    ]

instance RunMessage OrcTrickster where
  runMessage msg e@(OrcTrickster attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> immediateAttack pid attrs.id
      pure e
    -- it goes back on top of the encounter deck, and the player takes it from there
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      cancelDamage attrs
      players <- getRemainingPlayers
      unless (null players) do
        pid <- uniform players
        returnToTopOfEncounterDeck attrs
        takeHiddenCard pid
      pure e
    _ -> OrcTrickster <$> runMessage msg attrs
