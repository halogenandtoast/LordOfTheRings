module LordOfTheRings.Treachery.Cards.FuriousRainstorm (furiousRainstorm) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FuriousRainstorm = FuriousRainstorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

furiousRainstorm :: TreacheryCard FuriousRainstorm
furiousRainstorm = treachery FuriousRainstorm Cards.furiousRainstorm

-- | When Revealed: Each player must choose: raise his threat by the number of his
-- characters committed to the quest, or deal 1 damage to each of his characters committed
-- to the quest.
-- Shadow: If this attack destroys a character, raise your threat by the attacking enemy's
-- threat.
instance RunMessage FuriousRainstorm where
  runMessage msg t@(FuriousRainstorm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        committed <- selectCharacters (HeroControlledBy (PlayerWithId p) <> CommittedHero) (AllyControlledBy (PlayerWithId p) <> CommittedAlly)
        unless (null committed) $ chooseOneM p do
          questionLabeled "furiousRainstorm.choose"
          causedBy attrs
          labeledWith "furiousRainstorm.threat" [intVar "count" (length committed)] $ encounterRaisesThreat p (length committed)
          labeled "furiousRainstorm.damage" $ for_ committed \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      encounterRaisesThreat pid =<< getEnemyThreat eid
      pure t
    _ -> FuriousRainstorm <$> runMessage msg attrs
