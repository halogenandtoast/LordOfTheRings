module LordOfTheRings.Treachery.Cards.ObsidianArrows (obsidianArrows) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ObsidianArrows = ObsidianArrows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

obsidianArrows :: TreacheryCard ObsidianArrows
obsidianArrows = treachery ObsidianArrows Cards.obsidianArrows

-- | When Revealed: The current quest gains archery 2 until the end of the round. Then,
-- assign all archery damage to characters in play as if it were the beginning of the
-- combat phase.
-- Shadow: Attacking enemy gets +X attack. X is that enemy's archery value.
instance RunMessage ObsidianArrows where
  runMessage msg t@(ObsidianArrows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (\q -> roundModifier attrs q (GainKeyword (Archery 2))) =<< getCurrentQuest
      push ResolveArchery
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      archery <- getArchery (toTarget eid) . toCardDef =<< getEnemy eid
      when (archery > 0) $ enemyGetsAttack attrs eid archery
      pure t
    _ -> ObsidianArrows <$> runMessage msg attrs
