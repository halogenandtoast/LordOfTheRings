module LordOfTheRings.Ally.Cards.IthilienLookout (ithilienLookout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))

newtype IthilienLookout = IthilienLookout AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Secrecy 2, on its card def.
ithilienLookout :: AllyCard IthilienLookout
ithilienLookout = ally IthilienLookout Cards.ithilienLookout

-- | Response: After this ally enters play, look at the top card of the encounter deck. If
-- it is an enemy, you may discard it.
instance HasAbilities IthilienLookout where
  getAbilities (IthilienLookout a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage IthilienLookout where
  runMessage msg s@(IthilienLookout a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lookAtTopOfEncounterDeckMayDiscard pid a EnemyType
      pure s
    _ -> IthilienLookout <$> runMessage msg a
