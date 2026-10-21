{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Treachery (module LordOfTheRings.Treachery, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Treachery.Cards.CaughtInAWeb
import LordOfTheRings.Treachery.Cards.Despair
import LordOfTheRings.Treachery.Cards.DrivenByShadow
import LordOfTheRings.Treachery.Cards.EvilStorm
import LordOfTheRings.Treachery.Cards.EyesOfTheForest
import LordOfTheRings.Treachery.Cards.IronShackles
import LordOfTheRings.Treachery.Cards.LingeringVenom
import LordOfTheRings.Treachery.Cards.MassingAtNight
import LordOfTheRings.Treachery.Cards.PursuedByShadow
import LordOfTheRings.Treachery.Cards.TheNecromancersReach
import LordOfTheRings.Treachery.Cards.TreacherousFog
import LordOfTheRings.Treachery.Cards.UnderTheShadow
import LordOfTheRings.Treachery.Cards.FalseLead
import LordOfTheRings.Treachery.Cards.Flooding
import LordOfTheRings.Treachery.Cards.OldWivesTales
import LordOfTheRings.Treachery.Cards.AFrightenedBeast
import LordOfTheRings.Treachery.Cards.Sacked
import LordOfTheRings.Treachery.Cards.RoastedSlowly
import LordOfTheRings.Treachery.Runner ()
import LordOfTheRings.Treachery.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored treachery is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Treachery where
  parseJSON v = do
    attrs <- parseJSON @TreacheryAttrs v
    pure $ overAttrs (const attrs) $ lookupTreachery (treacheryCardCode attrs) (treacheryCardId attrs) (treacheryId attrs)

instance RunMessage Treachery where
  runMessage msg (Treachery a) = Treachery <$> runMessage msg a

createTreachery :: EncounterCard -> TreacheryId -> Treachery
createTreachery card = lookupTreachery (toCardCode card) (ecId card)

lookupTreachery :: (HasCallStack) => CardCode -> CardId -> TreacheryId -> Treachery
lookupTreachery cardCode cardId ident = case Map.lookup cardCode allTreacherys of
  Just (SomeTreacheryCard builder) -> Treachery $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown treachery: " <> show cardCode

newTreachery :: (MonadRandom m) => EncounterCard -> m Treachery
newTreachery card = createTreachery card <$> getRandom

allTreacherys :: Map CardCode SomeTreacheryCard
allTreacherys =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeTreacheryCard builder) <-
        [ SomeTreacheryCard eyesOfTheForest
        , SomeTreacheryCard caughtInAWeb
        , SomeTreacheryCard despair
        , SomeTreacheryCard drivenByShadow
        , SomeTreacheryCard theNecromancersReach
        , SomeTreacheryCard underTheShadow
        , SomeTreacheryCard ironShackles
        , SomeTreacheryCard massingAtNight
        , SomeTreacheryCard evilStorm
        , SomeTreacheryCard pursuedByShadow
        , SomeTreacheryCard treacherousFog
        , SomeTreacheryCard lingeringVenom
        , SomeTreacheryCard falseLead
        , SomeTreacheryCard flooding
        , SomeTreacheryCard oldWivesTales
        , SomeTreacheryCard aFrightenedBeast
        , SomeTreacheryCard sacked
        , SomeTreacheryCard roastedSlowly
        ]
    ]
