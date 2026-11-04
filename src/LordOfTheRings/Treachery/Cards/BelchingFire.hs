module LordOfTheRings.Treachery.Cards.BelchingFire (belchingFire) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Trait (Trait (Dragon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BelchingFire = BelchingFire TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Its burgle effect is read by burgle attempts only.
belchingFire :: TreacheryCard BelchingFire
belchingFire = treachery BelchingFire Cards.belchingFire

{- | When Revealed: Each Dragon enemy in the staging area makes an immediate attack against
the player with the highest total willpower committed to the quest. (Do not deal a shadow
card for this attack.) The first player chooses among players tied for it.
-}
instance RunMessage BelchingFire where
  runMessage msg t@(BelchingFire attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      dragons <- select (EnemyInStagingArea <> EnemyWithTrait Dragon)
      unless (null dragons) do
        players <- getRemainingPlayers
        totals <- for players \pid -> do
          committed <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId pid)) (CommittedAlly <> AllyControlledBy (PlayerWithId pid))
          (pid,) . sum <$> traverse getWillpower committed
        let highest = foldr (max . snd) 0 totals
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "belchingFire.player"
          causedBy attrs
          targets [pid | (pid, total) <- totals, total == highest] \pid ->
            for_ dragons (immediateAttackWithoutShadow pid)
      pure t
    _ -> BelchingFire <$> runMessage msg attrs
