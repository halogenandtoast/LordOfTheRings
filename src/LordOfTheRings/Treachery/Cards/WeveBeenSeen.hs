module LordOfTheRings.Treachery.Cards.WeveBeenSeen (weveBeenSeen) where

import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (getCityGuard)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeveBeenSeen = WeveBeenSeen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weveBeenSeen :: TreacheryCard WeveBeenSeen
weveBeenSeen = treachery WeveBeenSeen Cards.weveBeenSeen

{- | When Revealed: Each player must choose: either City Guard makes an attack against you,
or raise your threat by X, where X is 1 more than the number of allies you control.
Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, excess damage
must be assigned among characters you control: the damage past the destroyed character's
hit points goes to the defending player's other characters, as Cave Troll's does.
-}
instance RunMessage WeveBeenSeen where
  runMessage msg t@(WeveBeenSeen attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      guard' <- getCityGuard
      eachPlayer \p -> do
        x <- (+ 1) . length <$> select (AllyControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "weveBeenSeen.choose"
          causedBy attrs
          for_ guard' \g -> labeled "weveBeenSeen.attack" $ immediateAttack p g
          labeledWith "weveBeenSeen.threat" [intVar "n" x] $ encounterRaisesThreat p x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      createEffect attrs eid [ExcessDamageToOtherCharacters] UntilEndOfAttack
      pure t
    _ -> WeveBeenSeen <$> runMessage msg attrs
