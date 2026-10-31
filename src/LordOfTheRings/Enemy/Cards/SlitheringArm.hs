module LordOfTheRings.Enemy.Cards.SlitheringArm (slitheringArm) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SlitheringArm = SlitheringArm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slitheringArm :: EnemyCard SlitheringArm
slitheringArm = enemy SlitheringArm Cards.slitheringArm

-- | Surge. Grapple (most enemies grappled).
-- Forced: After Slithering Arm engages a player, it makes an immediate attack against that
-- player.
instance HasAbilities SlitheringArm where
  getAbilities (SlitheringArm a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Either resolve Slithering Arm's grapple keyword, or treat this attack as
-- undefended.
instance RunMessage SlitheringArm where
  runMessage msg e@(SlitheringArm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseOneM pid do
        questionLabeled "slitheringArm.choose"
        causedBy attrs
        labeled "slitheringArm.grapple" $ grappleShadowCard eid (MkEncounterCard cid (toCardCode attrs))
        labeled "slitheringArm.undefended" $ makeUndefended pid eid
      pure e
    _ -> SlitheringArm <$> runMessage msg attrs
