module LordOfTheRings.Ally.Cards.WardenOfAnnuminas (wardenOfAnnuminas) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype WardenOfAnnuminas = WardenOfAnnuminas AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

wardenOfAnnuminas :: AllyCard WardenOfAnnuminas
wardenOfAnnuminas = ally WardenOfAnnuminas Cards.wardenOfAnnuminas

-- | Warden of Annúminas gets +1 willpower for each enemy engaged with you.
instance HasModifiersFor WardenOfAnnuminas where
  getModifiersFor (WardenOfAnnuminas a) = for_ (placementController a.placement) \pid -> do
    n <- selectCount (EnemyEngagedWith $ PlayerWithId pid)
    when (n > 0) $ modifySelf a [WillpowerModifier n]
