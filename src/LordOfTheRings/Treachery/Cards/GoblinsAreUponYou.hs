module LordOfTheRings.Treachery.Cards.GoblinsAreUponYou (goblinsAreUponYou) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Trait (Trait (Goblin))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GoblinsAreUponYou = GoblinsAreUponYou TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinsAreUponYou :: TreacheryCard GoblinsAreUponYou
goblinsAreUponYou = treachery GoblinsAreUponYou Cards.goblinsAreUponYou

-- | While it is being revealed, its "when revealed" effect cannot be canceled.
instance HasModifiersFor GoblinsAreUponYou where
  getModifiersFor (GoblinsAreUponYou a) = when (a.placement == Unplaced) $ modified a GameTarget [CannotCancelEncounterEffects]

{- | When Revealed: Each player must search the encounter deck and discard pile for a Goblin
enemy and put it into play, engaged with him. Then, shuffle the encounter deck. (This
effect cannot be canceled.)
Shadow: Attacking enemy gets +1 attack for each Goblin enemy engaged with you.
-}
instance RunMessage GoblinsAreUponYou where
  runMessage msg t@(GoblinsAreUponYou attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscardToEngage pid attrs (CardWithType EnemyType <> CardWithTrait Goblin)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      n <- length <$> select (EnemyWithTrait Goblin <> EnemyEngagedWith (PlayerWithId pid))
      when (n > 0) $ enemyGetsAttack (CardIdSource cid) eid n
      pure t
    _ -> GoblinsAreUponYou <$> runMessage msg attrs
