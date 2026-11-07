module LordOfTheRings.Ally.Cards.HostOfLorien (hostOfLorien) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype HostOfLorien = HostOfLorien AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

hostOfLorien :: AllyCard HostOfLorien
hostOfLorien = ally HostOfLorien Cards.hostOfLorien

instance HasModifiersFor HostOfLorien where
  getModifiersFor (HostOfLorien a) = modifySelf a [ImmuneToPlayerCardEffects, DoesNotExhaustToCommit, RemoveFromGameIfLeavesPlay]
