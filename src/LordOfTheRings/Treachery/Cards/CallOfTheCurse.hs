module LordOfTheRings.Treachery.Cards.CallOfTheCurse (callOfTheCurse) where

import LordOfTheRings.Helpers.IslandMap (activePosition, locationsLeftOf)
import LordOfTheRings.Trait (Trait (Undead))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CallOfTheCurse = CallOfTheCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

callOfTheCurse :: TreacheryCard CallOfTheCurse
callOfTheCurse = treachery CallOfTheCurse Cards.callOfTheCurse

-- | Until the end of the round, each Undead enemy gets +X attack and +X defense. X is the
-- number of locations to the left of the active location (in its row).
curse :: (ReverseQueue m, Sourceable source) => source -> m Int
curse source = do
  x <- length <$> (maybe (pure []) locationsLeftOf =<< activePosition)
  when (x > 0) do
    undead <- select (EnemyWithTrait Undead)
    for_ undead \e -> roundModifiers source e [AttackModifier x, DefenseModifier x]
  pure x

-- | When Revealed: the curse. If X is 0 or 1, Call of the Curse gains surge.
-- Shadow: Resolve the 'when revealed' effect of Call of the Curse.
-- ponytail: as a shadow it gains no surge
instance RunMessage CallOfTheCurse where
  runMessage msg t@(CallOfTheCurse attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- curse attrs
      when (x <= 1) revealEncounterCard
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      void $ curse (CardIdSource cid)
      pure t
    _ -> CallOfTheCurse <$> runMessage msg attrs
