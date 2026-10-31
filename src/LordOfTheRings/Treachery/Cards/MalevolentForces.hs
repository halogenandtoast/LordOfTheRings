module LordOfTheRings.Treachery.Cards.MalevolentForces (malevolentForces) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Trait (Trait (Undead))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MalevolentForces = MalevolentForces TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

malevolentForces :: TreacheryCard MalevolentForces
malevolentForces = treachery MalevolentForces Cards.malevolentForces

-- | When Revealed: Remove the questing character with the highest willpower from the
-- quest. The player who controls that character raises his threat by an amount equal to
-- that character's willpower. Of those tied, the first player chooses.
-- Shadow: If this attack destroys a character, heal all damage from each Undead enemy in
-- play.
instance RunMessage MalevolentForces where
  runMessage msg t@(MalevolentForces attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      questing <- selectCharacters CommittedHero CommittedAlly
      scored <- for questing \c -> (c,) <$> getWillpower c
      let highest = maximumMay (map snd scored)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "malevolentForces.remove"
        causedBy attrs
        targets [c | (c, w) <- scored, Just w == highest] \c -> do
          removeFromQuest c
          controller <- characterController c
          for_ controller \p -> encounterRaisesThreat p (fromMaybe 0 highest)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      undead <- select (EnemyWithTrait Undead)
      for_ undead \e -> healAllDamage (CardIdSource cid) e
      pure t
    _ -> MalevolentForces <$> runMessage msg attrs
