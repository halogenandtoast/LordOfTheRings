module LordOfTheRings.Location.Cards.DesolationOfSmaug (desolationOfSmaug) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardDef (cdKeywords), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype DesolationOfSmaug = DesolationOfSmaug LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Its burgle effect is read by burgle attempts only.
desolationOfSmaug :: LocationCard DesolationOfSmaug
desolationOfSmaug = location DesolationOfSmaug Cards.desolationOfSmaug

{- | While Desolation of Smaug is the active location, the first treachery card revealed
each round gains surge. Read off the window after it is revealed, as Dark Pathway's: the
first is the one revealed while no other has been this round. One printed with surge
already surges, and gains nothing.
-}
instance RunMessage DesolationOfSmaug where
  runMessage msg (DesolationOfSmaug a) = case msg of
    CheckWindows ws | a.placement == ActiveLocation -> do
      inPlay <- getsGame (.entities.treacheries)
      revealedThisRound <- getsGame (length . filter (elem TreacheryRevealedThisRound . effectModifiers) . gameEffects)
      let revealed = mapMaybe (`Map.lookup` inPlay) [tid | Window After (Revealed (TreacheryTarget tid)) <- ws]
      when (revealedThisRound == 1 && any ((Surge `notMember`) . cdKeywords . toCardDef) revealed) revealEncounterCard
      DesolationOfSmaug <$> runMessage msg a
    _ -> DesolationOfSmaug <$> runMessage msg a
