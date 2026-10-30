module LordOfTheRings.Ally.Cards.Eothain (eothain) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype Eothain = Eothain AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eothain :: AllyCard Eothain
eothain = ally Eothain Cards.eothain

-- | Response: After a Rohan ally is discarded from play by a card effect, ready Éothain.
instance HasAbilities Eothain where
  getAbilities (Eothain a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly)
        $ response
        $ AllyWithTraitDiscardedByCardEffectWindow After Rohan
    ]

instance RunMessage Eothain where
  runMessage msg s@(Eothain a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> Eothain <$> runMessage msg a
