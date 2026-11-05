module LordOfTheRings.Treachery.Cards.SecretCultist (secretCultist) where

import LordOfTheRings.Helpers.Character (getAttack)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SecretCultist = SecretCultist TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

secretCultist :: TreacheryCard SecretCultist
secretCultist = treachery SecretCultist Cards.secretCultist

-- | Counts as a Condition attachment.
instance HasModifiersFor SecretCultist where
  getModifiersFor (SecretCultist a) = case a.placement of
    AttachedTo _ -> modifySelf a [GainTrait Condition]
    _ -> pure ()

{- | When Revealed: Attach to the highest attack ally in play. (Counts as a Condition
attachment with the text: "Attached ally loses the ally card type, gains the enemy card
type, and engages its owner. Treat attached ally's text box as if it were blank. If Secret
Cultist is discarded, or the attached ally's owner is eliminated, discard attached ally.")
The first player picks among those tied. The ally turns as Secret Cultist attaches: what
was attached to it as an ally goes, and Secret Cultist stays on the enemy it became. With
no ally in play there is nothing to attach to. Its owner eliminated, the ally leaves play
with the rest of his cards.
-}
instance RunMessage SecretCultist where
  runMessage msg t@(SecretCultist attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- traverse (\a -> (a,) <$> getAttack (toTarget a)) =<< select (AllyControlledBy AnyPlayer)
      let highest = foldr (max . snd) 0 allies
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "secretCultist.ally"
        causedBy attrs
        targets [a | (a, n) <- allies, n == highest] \a -> do
          allyBecomesEnemy a
          attachTreachery attrs.treacheryId (EnemyId $ unAllyId a)
      pure t
    Discard _ (isTarget attrs -> True) -> do
      case attrs.placement of
        AttachedTo host@(EnemyTarget eid) -> whenM (selectAny $ EnemyWithId eid) $ discard attrs host
        _ -> pure ()
      SecretCultist <$> runMessage msg attrs
    _ -> SecretCultist <$> runMessage msg attrs
