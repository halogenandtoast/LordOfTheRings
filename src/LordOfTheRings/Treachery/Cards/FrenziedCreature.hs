module LordOfTheRings.Treachery.Cards.FrenziedCreature (frenziedCreature) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FrenziedCreature = FrenziedCreature TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

frenziedCreature :: TreacheryCard FrenziedCreature
frenziedCreature = treachery FrenziedCreature Cards.frenziedCreature

-- | Counts as a Condition attachment with the text: "Attached enemy gets +1 threat, +1
-- attack, +1 defense and is immune to player card effects."
instance HasModifiersFor FrenziedCreature where
  getModifiersFor (FrenziedCreature a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> modified a host [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, ImmuneToPlayerCardEffects]
    _ -> pure ()

-- | When Revealed: Add the top card of the Evil Creatures deck to the staging area and
-- attach Frenzied Creature to it.
-- Shadow: Attach this card to attacking enemy.
instance RunMessage FrenziedCreature where
  runMessage msg t@(FrenziedCreature attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addFromEvilCreaturesDeck attrs
      pure t
    AddedFromEvilCreaturesDeck (isSource attrs -> True) card -> do
      enemies <- select (EnemyWithCardId card.id)
      for_ enemies $ attachTreachery attrs.treacheryId
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      attachShadowTreachery eid cid (EnemyTarget eid)
      pure t
    _ -> FrenziedCreature <$> runMessage msg attrs
