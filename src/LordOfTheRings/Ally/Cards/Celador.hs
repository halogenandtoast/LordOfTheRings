module LordOfTheRings.Ally.Cards.Celador (celador) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype Celador = Celador AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, once The Hidden Way gives him to the first player.
celador :: AllyCard Celador
celador = ally Celador Cards.celador

-- | Forced: After players quest unsuccessfully or a character leaves play, deal 1 damage
-- to Celador.
instance HasAbilities Celador where
  getAbilities (Celador a) =
    [ mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After
    , mkAbility a 2 $ forced $ CharacterLeftPlayWindow After AnyCard
    ]

-- | If Celador leaves play, remove him from the game.
instance RunMessage Celador where
  runMessage msg s@(Celador a) = case msg of
    UseCardAbility _ (isSource a -> True) _ _ -> do
      dealDamage a a 1
      pure s
    Discard (isTarget a -> True) -> do
      removeFromEncounterDiscard $ MkEncounterCard (allyCardId a) (toCardCode a)
      Celador <$> runMessage msg a
    _ -> Celador <$> runMessage msg a
