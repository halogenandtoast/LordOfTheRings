module LordOfTheRings.Treachery.Cards.BladeTip (bladeTip) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BladeTip = BladeTip TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

bladeTip :: TreacheryCard BladeTip
bladeTip = treachery BladeTip Cards.bladeTip

instance HasModifiersFor BladeTip where
  getModifiersFor (BladeTip a) = withHost a $ \_ -> modifySelf a [GainTrait Condition]

instance HasAbilities BladeTip where
  getAbilities (BladeTip a) = case a.placement of
    AttachedTo _ -> [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]
    _ -> []

instance RunMessage BladeTip where
  runMessage msg t@(BladeTip a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      withAttackDamage eid [DoStep 1 msg]
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid defenders) | cid == a.cardId -> do
      damaged <- attackDamaged eid
      present <- selectCharacters AnyHero AnyAlly
      stillDefending <- filterM (fmap (elem Defending) . getModifiers) defenders
      let eligible = [d | d <- stillDefending, d `elem` present, any (\(target, n) -> target == d && n > 0) damaged]
      chooseOrRunOneM pid do
        questionLabeled "bladeTip.attach"
        causedBy a
        targets eligible $ \d -> push $ PutShadowTreacheryIntoPlay eid cid (AttachedTo d)
      pure t
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a $ \host -> characterController host >>= traverse_ (`encounterRaisesThreat` 2)
      pure t
    _ -> BladeTip <$> runMessage msg a
