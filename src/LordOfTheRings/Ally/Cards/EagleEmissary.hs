module LordOfTheRings.Ally.Cards.EagleEmissary (eagleEmissary) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype EagleEmissary = EagleEmissary AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

eagleEmissary :: AllyCard EagleEmissary
eagleEmissary = ally EagleEmissary Cards.eagleEmissary

-- | Cannot have restricted attachments.
instance HasModifiersFor EagleEmissary where
  getModifiersFor (EagleEmissary a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Forced: After Eagle Emissary quests successfully, pay 1 Tactics resource or discard it
-- from play.
instance HasAbilities EagleEmissary where
  getAbilities (EagleEmissary a) =
    [restricted a 1 (exists $ AllyWithId a.id <> CommittedAlly) $ forced $ QuestedSuccessfullyWindow After]

instance RunMessage EagleEmissary where
  runMessage msg s@(EagleEmissary a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      payOrDiscard a.owner a Tactics
      pure s
    _ -> EagleEmissary <$> runMessage msg a
